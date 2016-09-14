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
package cz.cvut.kbss.jopa.model.metamodel;

import cz.cvut.kbss.jopa.exception.MetamodelInitializationException;
import cz.cvut.kbss.jopa.query.NamedQueryManager;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.*;

public class MetamodelBuilder {

    private static final Logger LOG = LoggerFactory.getLogger(MetamodelBuilder.class);

    private final NamedNativeQueryProcessor queryProcessor;

    private final Map<Class<?>, EntityType<?>> typeMap = new HashMap<>();
    private final Set<Class<?>> inferredClasses = new HashSet<>();
    private final NamedQueryManager namedQueryManager = new NamedQueryManager();

    public MetamodelBuilder() {
        this.queryProcessor = new NamedNativeQueryProcessor(namedQueryManager);
    }

    /**
     * Builds persistence unit metamodel from the specified set of entities.
     *
     * @param entities Entities declared for the persistence unit
     */
    public void buildMetamodel(Set<Class<?>> entities) {
        // AJC won't compile a method reference here
        entities.forEach(cls -> processOWLClass(cls));
    }

    private <X> void processOWLClass(final Class<X> cls) {
        if (typeMap.containsKey(cls)) {
            return;
        }

        LOG.debug("Processing OWL class: {}", cls);

        final EntityTypeImpl<X> et = EntityClassProcessor.processEntityType(cls);

        typeMap.put(cls, et);
        final EntityFieldMetamodelProcessor<X> fieldProcessor = new EntityFieldMetamodelProcessor<>(cls, et, this);

        EntityClassProcessor.getEntityFields(cls).forEach(fieldProcessor::processField);

        if (et.getIdentifier() == null) {
            throw new MetamodelInitializationException("Missing identifier field in entity class " + cls);
        }
        queryProcessor.processClass(cls);
    }

    public Map<Class<?>, EntityType<?>> getTypeMap() {
        return Collections.unmodifiableMap(typeMap);
    }

    public Set<Class<?>> getInferredClasses() {
        return Collections.unmodifiableSet(inferredClasses);
    }

    public NamedQueryManager getNamedQueryManager() {
        return namedQueryManager;
    }

    void addInferredClass(Class<?> cls) {
        inferredClasses.add(cls);
    }

    @SuppressWarnings("unchecked")
    <X> EntityType<X> getEntityClass(Class<X> cls) {
        if (!typeMap.containsKey(cls)) {
            processOWLClass(cls);
        }
        return (EntityType<X>) typeMap.get(cls);
    }
}
