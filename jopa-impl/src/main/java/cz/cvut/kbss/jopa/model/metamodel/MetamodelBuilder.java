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
package cz.cvut.kbss.jopa.model.metamodel;

import cz.cvut.kbss.jopa.exception.MetamodelInitializationException;
import cz.cvut.kbss.jopa.model.annotations.Inheritance;
import cz.cvut.kbss.jopa.query.NamedQueryManager;
import cz.cvut.kbss.jopa.utils.Constants;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.lang.reflect.Field;
import java.util.*;

public class MetamodelBuilder {

    private static final Logger LOG = LoggerFactory.getLogger(MetamodelBuilder.class);

    private final NamedNativeQueryProcessor queryProcessor;

    private final Map<Class<?>, AbstractIdentifiableType<?>> typeMap = new HashMap<>();
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
        entities.forEach(this::processOWLClass);
    }

    private <X> void processOWLClass(final Class<X> cls) {
        if (typeMap.containsKey(cls)) {
            return;
        }

        LOG.debug("Processing OWL class: {}", cls);

        final EntityTypeImpl<X> et = ManagedClassProcessor.processEntityType(cls);

        processManagedType(et);
    }

    private <X> void processManagedType(AbstractIdentifiableType<X> type) {
        final Class<X> cls = type.getJavaType();
        typeMap.put(cls, type);
        final ClassFieldMetamodelProcessor<X> fieldProcessor = new ClassFieldMetamodelProcessor<>(cls, type, this);

        for (Field f : cls.getDeclaredFields()) {
            fieldProcessor.processField(f);
        }

        final AbstractIdentifiableType<? super X> supertype = processSupertypes(cls);
        if (supertype != null) {
            type.setSupertype(supertype);
        }

        if (type.getPersistenceType() == Type.PersistenceType.ENTITY) {
            try {
                type.getIdentifier();
            } catch (IllegalArgumentException e) {
                throw new MetamodelInitializationException("Missing identifier field in entity class " + cls);
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
            final AbstractIdentifiableType<? super X> type = ManagedClassProcessor.processManagedType(managedSupertype);
            processManagedType(type);
            return type;
        }
        return null;
    }

    private <X> void resolveInheritanceType(EntityTypeImpl<X> et) {
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
        return namedQueryManager;
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
