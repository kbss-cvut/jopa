/**
 * Copyright (C) 2016 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.model;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.URL;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Default implementation of the {@link PersistenceProviderResolver}, threadsafe.
 * <p>
 * It gets persistence providers by scanning the classpath, looking for {@link #PROVIDER_FILE} containing fully
 * qualified name of a {@link PersistenceProvider} implementation.
 */
public class DefaultPersistenceProviderResolver implements PersistenceProviderResolver {

    private static final Logger LOG = LoggerFactory.getLogger(DefaultPersistenceProviderResolver.class);

    /**
     * Configuration file of the persistence provider implementations.
     */
    public static final String PROVIDER_FILE = "META-INF/services/" + PersistenceProperties.JPA_PERSISTENCE_PROVIDER;
    private static final Pattern nonCommentPattern = Pattern.compile("^([^#]+)");

    private static final Set<Class<? extends PersistenceProvider>> PROVIDER_TYPES = new LinkedHashSet<>(4);

    private List<PersistenceProvider> providers = null;

    @Override
    public synchronized List<PersistenceProvider> getPersistenceProviders() {
        if (providers == null) {
            this.providers = initProviders();
        }
        return Collections.unmodifiableList(providers);
    }

    private List<PersistenceProvider> initProviders() {
        final List<Class<? extends PersistenceProvider>> providerTypes = new ArrayList<>(PROVIDER_TYPES);
        providerTypes.addAll(resolveProviders());
        final List<PersistenceProvider> providerList = new ArrayList<>(providerTypes.size());
        for (Class<? extends PersistenceProvider> cls : providerTypes) {
            try {
                providerList.add(cls.newInstance());
            } catch (InstantiationException | IllegalAccessException e) {
                LOG.error("Unable to instantiate PersistenceProvider {}.", cls, e);
            }
        }
        if (providerList.isEmpty()) {
            LOG.warn("No persistence provider implementations found on classpath.");
        }
        return providerList;
    }

    @Override
    public synchronized void clearCachedProviders() {
        this.providers = null;
    }

    /**
     * Registers the specified class so that {@link PersistenceProvider} instances can be created by this resolver.
     * <p>
     * This allows to programmatically specify additional persistence providers. Providers registered via this method
     * are instantiated first by the resolver instance.
     *
     * @param cls The class to register
     */
    public static synchronized void registerPersistenceProviderClass(Class<? extends PersistenceProvider> cls) {
        PROVIDER_TYPES.add(cls);
    }

    private List<Class<? extends PersistenceProvider>> resolveProviders() {
        final ClassLoader classLoader = Thread.currentThread().getContextClassLoader();
        final List<Class<? extends PersistenceProvider>> providerTypes = new ArrayList<>();
        try {
            final Enumeration<URL> configs = classLoader.getResources(PROVIDER_FILE);

            while (configs.hasMoreElements()) {
                final URL config = configs.nextElement();
                resolveProvider(config).ifPresent(providerTypes::add);
            }
        } catch (IOException e) {
            LOG.error("Unable to read persistence provider configuration files from classpath.", e);
        }
        return providerTypes;
    }

    private Optional<Class<? extends PersistenceProvider>> resolveProvider(URL file) {
        try (final BufferedReader in = new BufferedReader(new InputStreamReader(file.openStream()))) {
            String line;
            while ((line = in.readLine()) != null) {
                line = line.trim();
                Matcher m = nonCommentPattern.matcher(line);
                if (m.find()) {
                    final String providerClass = m.group().trim();
                    return getPersistenceProviderClass(providerClass);
                }
            }
            return Optional.empty();
        } catch (IOException e) {
            LOG.error("Unable to read persistence provider implementation from file {}.", file, e);
        }
        return Optional.empty();
    }

    private static Optional<Class<? extends PersistenceProvider>> getPersistenceProviderClass(String className) {
        try {
            final Class<?> cls = Class.forName(className);
            if (!PersistenceProvider.class.isAssignableFrom(cls)) {
                LOG.error("The registered type {} is not a PersistenceProvider implementation.", className);
                return Optional.empty();
            }
            return Optional.of((Class<? extends PersistenceProvider>) cls);
        } catch (ClassNotFoundException e) {
            LOG.error("Persistence provider type {} not found.", className, e);
            return Optional.empty();
        }
    }
}
