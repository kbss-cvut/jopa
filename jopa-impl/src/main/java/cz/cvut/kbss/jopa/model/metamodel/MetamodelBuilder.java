/**
 * Copyright (C) 2019 Czech Technical University in Prague
 * <p>
 * This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later
 * version.
 * <p>
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License along with this program. If not, see
 * <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.model.metamodel;

import cz.cvut.kbss.jopa.exception.MetamodelInitializationException;
import cz.cvut.kbss.jopa.loaders.PersistenceUnitClassFinder;
import cz.cvut.kbss.jopa.model.annotations.Inheritance;
import cz.cvut.kbss.jopa.query.NamedQueryManager;
import cz.cvut.kbss.jopa.query.ResultSetMappingManager;
import cz.cvut.kbss.jopa.query.mapper.ResultSetMappingProcessor;
import cz.cvut.kbss.jopa.utils.Constants;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.lang.reflect.Field;
import java.util.*;

public class MetamodelBuilder {

    private static final Logger LOG = LoggerFactory.getLogger(MetamodelBuilder.class);

    private final NamedNativeQueryProcessor queryProcessor = new NamedNativeQueryProcessor();
    private final ResultSetMappingProcessor mappingProcessor;

    private final Map<Class<?>, AbstractIdentifiableType<?>> typeMap = new HashMap<>();
    private final Set<Class<?>> inferredClasses = new HashSet<>();

    private final ConverterResolver converterResolver = new ConverterResolver(new Converters());

    public MetamodelBuilder() {
        this.mappingProcessor = new ResultSetMappingProcessor(this);
    }

    /**
     * Builds persistence unit metamodel based on classes discovered by the specified class finder.
     *
     * @param classFinder Holder of information about classes relevant for persistence unit building
     */
    public void buildMetamodel(PersistenceUnitClassFinder classFinder) {
        classFinder.getEntities().forEach(this::processOWLClass);
        classFinder.getResultSetMappings().forEach(mappingProcessor::buildMapper);
    }

    private <X> void processOWLClass(final Class<X> cls) {
        if (typeMap.containsKey(cls)) {
            return;
        }

        LOG.debug("Processing OWL class: {}", cls);

        final TypeBuilderContext<X> et = ManagedClassProcessor.processManagedType(cls);
        et.setConverterResolver(converterResolver);

        processManagedType(et);
    }

    private <X> void processManagedType(TypeBuilderContext<X> context) {
        final AbstractIdentifiableType<X> type = context.getType();
        final Class<X> cls = type.getJavaType();
        typeMap.put(cls, type);

        final AbstractIdentifiableType<? super X> supertype = processSupertypes(cls);
        if (supertype != null) {
            type.setSupertype(supertype);
        }
        type.setLifecycleListenerManager(new EntityLifecycleCallbackResolver(type).resolve());

        final ClassFieldMetamodelProcessor<X> fieldProcessor = new ClassFieldMetamodelProcessor<>(context, this);

        for (Field f : cls.getDeclaredFields()) {
            fieldProcessor.processField(f);
        }

        if (type.getPersistenceType() == Type.PersistenceType.ENTITY) {
            try {
                type.getIdentifier();
            } catch (IllegalArgumentException e) {
                throw new MetamodelInitializationException("Missing identifier field in entity " + cls);
            }
            resolveInheritanceType((EntityTypeImpl<X>) type);
        }

        queryProcessor.processClass(cls);
    }

    private <X> AbstractIdentifiableType<? super X> processSupertypes(Class<X> cls) {
        final Class<? super X> managedSupertype = ManagedClassProcessor.getManagedSupertype(cls);
        if (managedSupertype != null) {
            if (typeMap.containsKey(managedSupertype)) {
                return (AbstractIdentifiableType<? super X>) typeMap.get(managedSupertype);
            }
            final TypeBuilderContext<? super X> context = ManagedClassProcessor.processManagedType(managedSupertype);
            context.setConverterResolver(converterResolver);
            processManagedType(context);
            return context.getType();
        }
        return null;
    }

    private static <X> void resolveInheritanceType(EntityTypeImpl<X> et) {
        final Class<X> cls = et.getJavaType();
        final Inheritance inheritance = cls.getDeclaredAnnotation(Inheritance.class);
        if (inheritance != null) {
            if (et.getSupertype() != null &&
                    et.getSupertype().getPersistenceType() != Type.PersistenceType.MAPPED_SUPERCLASS) {
                throw new MetamodelInitializationException("Class " + cls +
                        " cannot declare inheritance strategy, because it already inherits it from its supertype.");
            }
            et.setInheritanceType(inheritance.strategy());
        } else {
            et.setInheritanceType(Constants.DEFAULT_INHERITANCE_TYPE);
        }
    }

    public Map<Class<?>, ManagedType<?>> getTypeMap() {
        return Collections.unmodifiableMap(typeMap);
    }

    public <X> AbstractIdentifiableType<X> entity(Class<X> cls) {
        return (AbstractIdentifiableType<X>) typeMap.get(cls);
    }

    public Map<Class<?>, EntityType<?>> getEntities() {
        final Map<Class<?>, EntityType<?>> map = new HashMap<>();
        typeMap.entrySet().stream().filter(e -> e.getValue().getPersistenceType() == Type.PersistenceType.ENTITY)
               .forEach(e -> map.put(e.getKey(), (EntityType<?>) e.getValue()));
        return map;
    }

    public Set<Class<?>> getInferredClasses() {
        return Collections.unmodifiableSet(inferredClasses);
    }

    public NamedQueryManager getNamedQueryManager() {
        return queryProcessor.getQueryManager();
    }

    public ResultSetMappingManager getResultSetMappingManager() {
        return mappingProcessor.getManager();
    }

    void addInferredClass(Class<?> cls) {
        inferredClasses.add(cls);
    }

    @SuppressWarnings("unchecked")
    <X> ManagedType<X> getEntityClass(Class<X> cls) {
        if (!typeMap.containsKey(cls)) {
            processOWLClass(cls);
        }
        return (ManagedType<X>) typeMap.get(cls);
    }
}
