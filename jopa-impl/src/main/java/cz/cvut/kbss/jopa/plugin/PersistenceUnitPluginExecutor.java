package cz.cvut.kbss.jopa.plugin;

import cz.cvut.kbss.jopa.exception.PluginException;
import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.model.JOPAPersistenceProperties;
import cz.cvut.kbss.jopa.utils.Configuration;

import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.function.Supplier;

/**
 * Resolves, instantiates and executes persistence unit lifecycle plugins.
 * <p>
 * The plugins are executed in the order they are defined in the configuration.
 */
public class PersistenceUnitPluginExecutor {

    private final List<PersistenceUnitLifecyclePlugin> plugins;

    public PersistenceUnitPluginExecutor(Configuration config) {
        Objects.requireNonNull(config);
        this.plugins = resolveAndLoadPlugins(config);
    }

    private static List<PersistenceUnitLifecyclePlugin> resolveAndLoadPlugins(Configuration config) {
        final String pluginClassesConfig = config.contains(JOPAPersistenceProperties.PERSISTENCE_UNIT_LIFECYCLE_PLUGINS)
                ? config.get(JOPAPersistenceProperties.PERSISTENCE_UNIT_LIFECYCLE_PLUGINS)
                : config.get(JOPAPersistenceProperties.PERSISTENCE_UNIT_LIFECYCLE_PLUGINS_LEGACY);
        final String[] pluginClasses = pluginClassesConfig.split(",");
        if (pluginClasses.length == 0 || pluginClassesConfig.isBlank()) {
            return List.of();
        }
        final List<PersistenceUnitLifecyclePlugin> plugins = new ArrayList<>(pluginClasses.length);
        for (String pluginClass : pluginClasses) {
            try {
                final Class<? extends PersistenceUnitLifecyclePlugin> cls = Class.forName(pluginClass.trim())
                                                                                 .asSubclass(PersistenceUnitLifecyclePlugin.class);
                plugins.add(cls.getConstructor().newInstance());
            } catch (ClassNotFoundException e) {
                throw new PluginException("Plugin class " + pluginClass + " not found.", e);
            } catch (InvocationTargetException | InstantiationException | IllegalAccessException |
                     NoSuchMethodException e) {
                throw new PluginException("Unable to instantiate plugin class " + pluginClass, e);
            }
        }
        return plugins;
    }

    public void afterPersistenceUnitCreated(Supplier<EntityManager> emSupplier) {
        plugins.forEach(plugin -> {
            try (EntityManager em = emSupplier.get()) {
                plugin.afterPersistenceUnitCreated(em);
            }
        });
    }

    public void beforePersistenceUnitDestroyed(Supplier<EntityManager> emSupplier) {
        plugins.forEach(plugin -> {
            try (EntityManager em = emSupplier.get()) {
                plugin.beforePersistenceUnitDestroyed(em);
            }
        });
    }
}
