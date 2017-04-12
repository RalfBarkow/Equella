package com.tle.core.services.guice;

import javax.inject.Singleton;

import com.tle.common.Check;
import com.tle.core.config.guice.MandatoryConfigModule;
import com.tle.core.config.guice.OptionalConfigModule;
import com.tle.core.events.EventExecutor;
import com.tle.core.events.listeners.ApplicationListener;
import com.tle.core.guice.PluginTrackerModule;
import com.tle.core.services.TaskService;
import com.tle.core.services.impl.ClusterMessageHandler;
import com.tle.core.services.impl.ClusteredTaskServiceImpl;
import com.tle.core.services.impl.LocalTaskServiceImpl;

@SuppressWarnings("nls")
public class CoreServicesModule extends MandatoryConfigModule
{
	@Override
	protected void configure()
	{
		bindURL("admin.url");
		install(new CoreServicesOptionalModule());
		install(new CoreServicesTrackerModule());
	}

	public static class CoreServicesOptionalModule extends OptionalConfigModule
	{
		@Override
		protected void configure()
		{
			bindInt("taskService.maxConcurrentTasks", 26);

			bindBoolean("can.access.internet");

			bindProp("messaging.bindAddress");
			bindInt("messaging.bindPort", 8999);

			if( Check.isEmpty(getProperty("zookeeper.instances")) )
			{
				bind(TaskService.class).to(LocalTaskServiceImpl.class).in(Singleton.class);
			}
			else
			{
				bind(TaskService.class).to(ClusteredTaskServiceImpl.class).in(Singleton.class);
			}
		}
	}

	public static class CoreServicesTrackerModule extends PluginTrackerModule
	{
		@Override
		protected void configure()
		{
			bindTracker(ApplicationListener.class, "applicationEventListener", null);
			bindTracker(EventExecutor.class, "eventExecutor", "bean");
			bindTracker(ClusterMessageHandler.class, "clusterMessageHandler", "bean");
			bindTracker(Object.class, "coreTasks", null).setIdParam("id");
		}
	}
}