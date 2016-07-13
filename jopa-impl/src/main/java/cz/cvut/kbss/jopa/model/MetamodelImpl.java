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

import cz.cvut.kbss.jopa.exceptions.OWLPersistenceException;
import cz.cvut.kbss.jopa.loaders.EntityLoader;
import cz.cvut.kbss.jopa.model.metamodel.*;
import cz.cvut.kbss.jopa.query.NamedQueryManager;
import cz.cvut.kbss.jopa.utils.Configuration;
import cz.cvut.kbss.ontodriver.config.OntoDriverProperties;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.net.URI;
import java.net.URISyntaxException;
import java.util.*;

public class MetamodelImpl implements Metamodel {

    private static final Logger LOG = LoggerFactory.getLogger(Metamodel.class);

    private static final String ASPECTJ_CLASS = "org.aspectj.weaver.loadtime.Agent";

    private Map<Class<?>, EntityType<?>> typeMap;
    private Set<Class<?>> inferredClasses;

    private NamedQueryManager namedQueryManager;

    private final Configuration configuration;

    private Set<URI> moduleExtractionSignature;

    public MetamodelImpl(Configuration configuration, EntityLoader entityLoader) {
        this.configuration = Objects.requireNonNull(configuration);
        Objects.requireNonNull(entityLoader);
        build(entityLoader);
    }

    private void build(EntityLoader entityLoader) {
        LOG.debug("Building metamodel...");
        checkForWeaver();

        final Set<Class<?>> discoveredEntities = entityLoader.discoverEntityClasses(configuration);

        final MetamodelBuilder metamodelBuilder = new MetamodelBuilder();
        metamodelBuilder.buildMetamodel(discoveredEntities);

        this.typeMap = metamodelBuilder.getTypeMap();
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
    public <X> EntityType<X> entity(Class<X> cls) {
        if (!typeMap.containsKey(cls)) {
            throw new IllegalArgumentException(
                    "Class " + cls.getName() + " is not a known entity in this persistence unit.");
        }
        return (EntityType<X>) typeMap.get(cls);
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
        return new HashSet<>(typeMap.values());
    }

    @Override
    public Set<ManagedType<?>> getManagedTypes() {
        return new HashSet<>(typeMap.values());
    }

    @Override
    public <X> ManagedType<X> managedType(Class<X> cls) {
        return entity(cls);
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
        if (uri == null) {
            throw new NullPointerException();
        }
        synchronized (this) {
            getSignatureInternal().add(uri);
        }
    }

    private synchronized Set<URI> getSignatureInternal() {
        // This can be lazily loaded since we don'attributeType know if we'll need it
        if (moduleExtractionSignature == null) {
            final String sig = configuration.get(OntoDriverProperties.MODULE_EXTRACTION_SIGNATURE);
            if (sig == null) {
                this.moduleExtractionSignature = new HashSet<>();
            } else {
                final String[] signature = sig.split(OntoDriverProperties.SIGNATURE_DELIMITER);
                this.moduleExtractionSignature = new HashSet<>(signature.length);
                try {
                    for (String uri : signature) {
                        moduleExtractionSignature.add(new URI(uri));
                    }
                } catch (URISyntaxException e) {
                    throw new OWLPersistenceException("Invalid URI encountered in module extraction signature.", e);
                }
            }
        }
        return moduleExtractionSignature;
    }
}
