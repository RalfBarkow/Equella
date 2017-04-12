package com.tle.web.freemarker;

import java.io.IOException;
import java.io.Reader;
import java.io.Writer;
import java.util.HashMap;
import java.util.Map;

import org.apache.log4j.Logger;

import com.dytech.edge.common.Constants;
import com.google.common.base.Throwables;
import com.tle.annotation.NonNullByDefault;
import com.tle.common.i18n.CurrentLocale;
import com.tle.common.i18n.CurrentTimeZone;
import com.tle.web.sections.SectionUtils;
import com.tle.web.sections.render.PreRenderable;

import freemarker.core.Environment;
import freemarker.template.Configuration;
import freemarker.template.Template;

@NonNullByDefault
public abstract class AbstractFreemarkerFactory
{
	private static final Logger LOGGER = Logger.getLogger(AbstractFreemarkerFactory.class);

	protected Configuration configuration;

	public Environment render(FreemarkerSectionResult result, Writer writer)
	{
		Thread currentThread = Thread.currentThread();
		ClassLoader origLoader = currentThread.getContextClassLoader();
		Template template = result.getTemplate();
		Map<String, Object> rootObjects = new HashMap<String, Object>();
		addRootObjects(rootObjects, result, writer);
		Map<String, Object> extraObjects = result.getExtraObjects();
		if( extraObjects != null )
		{
			rootObjects.putAll(extraObjects);
		}
		currentThread.setContextClassLoader(getContextClassLoader());
		try
		{
			Environment environment = template.createProcessingEnvironment(rootObjects, writer);
			setupEnvironment(writer, result, environment);
			environment.process();
			finishedRender(writer, result, environment);
			return environment;
		}
		catch( Throwable te )
		{
			LOGGER.error("Error rendering " + result.getTemplate().getName());
			throw Throwables.propagate(te);
		}
		finally
		{
			currentThread.setContextClassLoader(origLoader);
		}
	}

	protected ClassLoader getContextClassLoader()
	{
		return getClass().getClassLoader();
	}

	protected void setupEnvironment(Writer writer, FreemarkerSectionResult result, Environment environment)
	{
		environment.setLocale(CurrentLocale.getLocale());
		environment.setTimeZone(CurrentTimeZone.get());
		environment.setURLEscapingCharset(Constants.UTF8);
	}

	protected void finishedRender(Writer writer, FreemarkerSectionResult result, Environment environment)
	{
		// nothing
	}

	protected void addRootObjects(Map<String, Object> rootObjects, FreemarkerSectionResult result, Writer writer)
	{
		rootObjects.put("m", result.getModel()); //$NON-NLS-1$
	}

	public FreemarkerSectionResult createResult(String name, Reader reader, Object model)
	{
		return createResult(name, reader, model, null);
	}

	public FreemarkerSectionResult createResult(String name, Reader reader, Object model, PreRenderable preRenderer)
	{
		try
		{
			Template template = new Template(name, reader, configuration);
			FreemarkerSectionResult result = new FreemarkerSectionResult(this, template);
			result.setModel(model);
			result.setPreRenderer(preRenderer);
			return result;
		}
		catch( IOException e )
		{
			SectionUtils.throwRuntime(e);
			return null;
		}
	}
}