package com.tle.web.plugin.download;

import java.io.File;
import java.io.IOException;
import java.io.StringWriter;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import javax.inject.Singleton;

import org.java.plugin.registry.Extension;
import org.java.plugin.registry.PluginAttribute;
import org.java.plugin.registry.PluginDescriptor;
import org.java.plugin.util.IoUtil;

import com.google.common.base.Charsets;
import com.google.common.base.Throwables;
import com.google.common.collect.ImmutableSet;
import com.google.common.io.CharStreams;
import com.google.common.io.Resources;
import com.tle.common.filters.EqFilter;
import com.tle.core.guice.Bind;
import com.tle.core.plugins.AbstractPluginService.TLEPluginLocation;
import com.tle.core.plugins.PluginService;
import com.tle.core.remoting.RemotePluginDownloadService;
import com.tle.core.services.UrlService;

@Bind
@Singleton
public class PluginDownloadService implements RemotePluginDownloadService
{
	private String jarPath;

	@Inject
	private PluginService pluginService;
	@Inject
	private UrlService urlService;
	@SuppressWarnings("nls")
	private Set<String> DISALLOWED = ImmutableSet.of("com.tle.core.guice", "com.tle.core.spring", "org.hibernate",
		"org.springframework.httpinvoker", "com.tle.webstart.admin");

	/**
	 * Don't use directly - call getJarMap().
	 */
	private Map<String, TLEPluginLocation> jarMap;

	@Override
	@SuppressWarnings("nls")
	public List<PluginDetails> getAllPluginDetails(String pluginType)
	{
		final Set<PluginDescriptor> plugins = pluginService.getAllPluginsAndDependencies(new FilterByType(pluginType),
			DISALLOWED, false);
		final Map<String, TLEPluginLocation> manifestToLocation = pluginService.getPluginIdToLocation();

		List<PluginDetails> details = new ArrayList<PluginDetails>();
		for( PluginDescriptor desc : plugins )
		{
			TLEPluginLocation location = manifestToLocation.get(desc.getId());
			if( !pluginService.isPluginDisabled(location) )
			{
				StringWriter manWriter = new StringWriter();
				try
				{
					CharStreams.copy(Resources.newReaderSupplier(location.getManifestLocation(), Charsets.UTF_8),
						manWriter);

					URL jarUrl = location.getContextLocation();
					if( jarUrl.getProtocol().equals("jar") )
					{
						jarUrl = new URL("jar", "", new URL(urlService.getInstitutionUrl(), jarPath + location.getJar()
							+ "!/").toString());
					}
					details.add(new PluginDetails(jarUrl, manWriter.toString()));
				}
				catch( IOException e )
				{
					throw Throwables.propagate(e);
				}
			}
		}
		return details;
	}

	@SuppressWarnings("nls")
	@PostConstruct
	void setupMapping()
	{
		Extension extension = pluginService.getPluginForObject(getClass()).getDescriptor()
			.getExtension("downloadServletMapping");
		String jarFilePath = extension.getParameter("url-pattern").valueAsString(); //$NON-NLS-1$
		this.jarPath = jarFilePath.substring(1, jarFilePath.length() - 1);
	}

	private synchronized Map<String, TLEPluginLocation> getJarMap()
	{
		if( jarMap == null )
		{
			jarMap = new HashMap<String, TLEPluginLocation>();
			for( TLEPluginLocation loc : pluginService.getPluginIdToLocation().values() )
			{
				jarMap.put(loc.getJar(), loc);
			}
		}
		return jarMap;
	}

	public File getFileForJar(String jarFile)
	{
		TLEPluginLocation location = getJarMap().get(jarFile);
		if( location != null )
		{
			return IoUtil.url2file(location.getContextLocation());
		}
		return null;
	}

	private static class FilterByType extends EqFilter<PluginDescriptor>
	{
		public FilterByType(String pluginType)
		{
			super(pluginType);
		}

		@Override
		protected Object getForComparison(PluginDescriptor d)
		{
			PluginAttribute attr = d.getAttribute("type"); //$NON-NLS-1$
			return attr == null ? null : attr.getValue();
		}
	}
}