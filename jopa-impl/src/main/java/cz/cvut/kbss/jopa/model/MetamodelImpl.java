/**
 * Copyright (C) 2016 Czech Technical University in Prague
 * <p>
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 * <p>
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.model;

import cz.cvut.kbss.jopa.exceptions.OWLPersistenceException;
import cz.cvut.kbss.jopa.loaders.EntityLoader;
import cz.cvut.kbss.jopa.model.metamodel.*;
import cz.cvut.kbss.jopa.query.NamedQueryManager;
import cz.cvut.kbss.jopa.utils.Configuration;
import cz.cvut.kbss.ontodriver.config.OntoDriverProperties;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.net.URI;
import java.util.*;
import java.util.regex.Pattern;

public class MetamodelImpl implements Metamodel {

    private static final Logger LOG = LoggerFactory.getLogger(Metamodel.class);

    private static final String ASPECTJ_CLASS = "org.aspectj.weaver.loadtime.Agent";

    private Map<Class<?>, ManagedType<?>> typeMap;
    private Map<Class<?>, EntityType<?>> entities;
    private Set<Class<?>> inferredClasses;

    private NamedQueryManager namedQueryManager;

    private final Configuration configuration;

    private Set<URI> moduleExtractionSignature;

    protected MetamodelImpl() {
        // Protected constructor for easier mocking
        this.configuration = null;
    }

    public MetamodelImpl(Configuration configuration) {
        this.configuration = Objects.requireNonNull(configuration);
    }

    /**
     * Builds the metamodel for entities discovered by the specified entity loader.
     *
     * @param entityLoader Loader of entity classes
     */
    public void build(EntityLoader entityLoader) {
        Objects.requireNonNull(entityLoader);
        LOG.debug("Building metamodel...");
        checkForWeaver();

        final Set<Class<?>> discoveredEntities = entityLoader.discoverEntityClasses(configuration);

        final MetamodelBuilder metamodelBuilder = new MetamodelBuilder();
        metamodelBuilder.buildMetamodel(discoveredEntities);

        this.typeMap = metamodelBuilder.getTypeMap();
        this.entities = metamodelBuilder.getEntities();
        this.inferredClasses = metamodelBuilder.getInferredClasses();
        this.namedQueryManager = metamodelBuilder.getNamedQueryManager();
    }

    /**
     * Check the class path for aspectj weaver, which is vital for using lazy loading.
     */
    private void checkForWeaver() {
        try {
            MetamodelImpl.class.getClassLoader().loadClass(ASPECTJ_CLASS);
        } catch (ClassNotFoundException e) {
            LOG.error("AspectJ not found on classpath. Cannot run without AspectJ.");
            throw new OWLPersistenceException(e);
        }
    }

    @SuppressWarnings("unchecked")
    @Override
    public <X> EntityTypeImpl<X> entity(Class<X> cls) {
        if (!entities.containsKey(cls)) {
            throw new IllegalArgumentException(
                    "Class " + cls.getName() + " is not a known entity in this persistence unit.");
        }
        return (EntityTypeImpl<X>) typeMap.get(cls);
    }

    @Override
    public <X> EmbeddableType<X> embeddable(Class<X> cls) {
        throw new IllegalArgumentException("Embeddable entities not supported.");
    }

    @Override
    public Set<EmbeddableType<?>> getEmbeddables() {
        return Collections.emptySet();
    }

    @Override
    public Set<EntityType<?>> getEntities() {
        return new HashSet<>(entities.values());
    }

    @Override
    public Set<ManagedType<?>> getManagedTypes() {
        return new HashSet<>(typeMap.values());
    }

    @Override
    public Set<Class<?>> getInferredClasses() {
        return Collections.unmodifiableSet(inferredClasses);
    }

    public NamedQueryManager getNamedQueryManager() {
        return namedQueryManager;
    }

    @Override
    public Set<URI> getModuleExtractionExtraSignature() {
        return Collections.unmodifiableSet(getSignatureInternal());
    }

    @Override
    public void addUriToModuleExtractionSignature(URI uri) {
        Objects.requireNonNull(uri);
        synchronized (this) {
            getSignatureInternal().add(uri);
        }
    }

    private synchronized Set<URI> getSignatureInternal() {
        // This can be lazily loaded since we don'attributeType know if we'll need it
        if (moduleExtractionSignature == null) {
            final String sig = configuration.get(OntoDriverProperties.MODULE_EXTRACTION_SIGNATURE, "");
            if (sig.isEmpty()) {
                this.moduleExtractionSignature = new HashSet<>();
            } else {
                final String[] signature = sig.split(Pattern.quote(OntoDriverProperties.SIGNATURE_DELIMITER));
                this.moduleExtractionSignature = new HashSet<>((int) (signature.length / 0.75 + 1));
                try {
                    for (String uri : signature) {
                        moduleExtractionSignature.add(URI.create(uri));
                    }
                } catch (IllegalArgumentException e) {
                    throw new OWLPersistenceException("Invalid URI encountered in module extraction signature.", e);
                }
            }
        }
        return moduleExtractionSignature;
    }
}
