/*
 * JOPA
 * Copyright (C) 2025 Czech Technical University in Prague
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3.0 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library.
 */
package cz.cvut.kbss.jopa.model.metamodel;

import cz.cvut.kbss.jopa.exception.StaticMetamodelInitializationException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;

/**
 * Initializes static metamodel based on the provided runtime metamodel.
 * <p>
 * Static metamodel initialization involves going through all managed types in the metamodel, finding a corresponding
 * static metamodel class (if exists) and populating its attributes with values from the actual runtime metamodel.
 */
public class StaticMetamodelInitializer {

    private static final Logger LOG = LoggerFactory.getLogger(StaticMetamodelInitializer.class);

    /**
     * Suffix appended to a static metamodel class corresponding to a metamodel class.
     */
    private static final String STATIC_METAMODEL_CLASS_SUFFIX = "_";

    /**
     * Name of the field containing the entity class IRI in a static metamodel class.
     */
    private static final String ENTITY_CLASS_IRI_FIELD = "entityClassIRI";

    /**
     * Suffix added to the name of an attribute to create the name of the field containing the attribute property IRI in
     * a static metamodel class.
     */
    private static final String PROPERTY_IRI_SUFFIX = "PropertyIRI";

    private final Metamodel metamodel;

    public StaticMetamodelInitializer(Metamodel metamodel) {
        this.metamodel = metamodel;
    }

    /**
     * Executes the static metamodel initialization.
     */
    public void initializeStaticMetamodel() {
        LOG.debug("Initializing static metamodel.");
        processEntities();
        processManagedTypes();
    }

    private void processEntities() {
        metamodel.getEntities().forEach(this::processType);
    }

    private void processType(ManagedType<?> mt) {
        final Optional<Class<?>> smClass = tryFindingClass(mt);
        if (smClass.isEmpty()) {
            LOG.trace("No static metamodel type found for {}.", mt);
            return;
        }
        LOG.debug("Processing static metamodel class {} corresponding to {}.", smClass.get(), mt);
        verifyParents(smClass.get(), mt);
        try {
            initStaticMembers(mt, smClass.get());
        } catch (IllegalAccessException e) {
            throw new StaticMetamodelInitializationException("Unable to initialize static metamodel class " + smClass,
                    e);
        }
    }

    private static Optional<Class<?>> tryFindingClass(ManagedType<?> type) {
        final String staticName = type.getJavaType().getName() + STATIC_METAMODEL_CLASS_SUFFIX;
        try {
            final Class<?> smClass = Class.forName(staticName);
            if (isNotStaticMetamodelForType(smClass, type.getJavaType())) {
                return Optional.empty();
            }
            return Optional.of(smClass);
        } catch (ClassNotFoundException e) {
            // Swallow the exception, this just means there is no static metamodel type for the specified entity
            return Optional.empty();
        }
    }

    private static boolean isNotStaticMetamodelForType(Class<?> smClass, Class<?> metamodelClass) {
        return smClass.getAnnotation(StaticMetamodel.class) == null ||
                !smClass.getAnnotation(StaticMetamodel.class).value().equals(metamodelClass);
    }

    private static void verifyParents(Class<?> smClass, ManagedType<?> type) {
        if (type instanceof IdentifiableType<?> idType && !idType.getSupertypes().isEmpty()) {
            for (IdentifiableType<?> superType : idType.getSupertypes()) {
                final Optional<Class<?>> supertypeSm = tryFindingClass(superType);
                if (supertypeSm.isEmpty() || !Objects.equals(smClass.getSuperclass(), supertypeSm.get())) {
                    throw new StaticMetamodelInitializationException("Managed type " + type +
                            " has a managed supertype. A corresponding relationship must exist between static metamodel classes.");
                }
            }
        }
    }

    private <T> void initStaticMembers(ManagedType<T> et, Class<?> smClass) throws IllegalAccessException {

        final Field[] fields = smClass.getDeclaredFields();
        for (Field f : fields) {
            if (!isCanonicalMetamodelField(f)) {
                LOG.debug("Skipping field {}, it is not canonical (public static).", f);
                continue;
            }
            if (isEntityClassIRIField(f, et)) {
                setFieldValue(f, ((EntityType<T>) et).getIRI());
                continue;
            }
            if (isPropertyIRIField(f, et)) {
                setPropertyIriFieldValue(et, f);
                continue;
            }
            final FieldSpecification<T, ?> att = getMetamodelMember(f, et);
            setFieldValue(f, att);
        }
    }

    private static boolean isCanonicalMetamodelField(Field field) {
        return Modifier.isStatic(field.getModifiers()) && Modifier.isPublic(field.getModifiers());
    }

    private static boolean isEntityClassIRIField(Field field, ManagedType<?> type) {
        return type.getPersistenceType() == Type.PersistenceType.ENTITY && field.getName()
                                                                                .equals(ENTITY_CLASS_IRI_FIELD);
    }

    private static void setFieldValue(Field field, Object value) throws IllegalAccessException {
        assert isCanonicalMetamodelField(field);
        field.set(null, value);
    }

    private static boolean isPropertyIRIField(Field field, ManagedType<?> type) {
        return type.getPersistenceType() == Type.PersistenceType.ENTITY && field.getName()
                                                                                .endsWith(PROPERTY_IRI_SUFFIX);
    }

    private static <T> void setPropertyIriFieldValue(ManagedType<T> et, Field f) throws IllegalAccessException {
        final String attributeName = f.getName()
                                      .substring(0, f.getName().length() - PROPERTY_IRI_SUFFIX.length());
        final Optional<FieldSpecification<T, ?>> att = getDeclaredAttribute(attributeName, et);
        if (att.isPresent()) {
            setFieldValue(f, ((Attribute<T, ?>) att.get()).getIRI());
        }
    }

    private <T> FieldSpecification<T, ?> getMetamodelMember(Field field, ManagedType<T> type) {
        LOG.trace("Finding metamodel member for static metamodel field {}.", field);
        return getDeclaredIdentifier(field, type)
                .orElseGet(() -> getDeclaredAttribute(field, type)
                        .orElseGet(() -> getDeclaredTypes(field, type)
                                .orElseGet(() -> getDeclaredProperties(field, type)
                                        .orElseThrow(() -> new StaticMetamodelInitializationException(
                                                "No corresponding metamodel member found for static metamodel field " +
                                                        field)))));
    }

    private static <T> Optional<FieldSpecification<T, ?>> getDeclaredIdentifier(Field field, ManagedType<T> type) {
        if (!(type instanceof IdentifiableType<T> mt)) {
            return Optional.empty();
        }
        return Objects.equals(field.getName(), mt.getIdentifier().getJavaField().getName()) &&
                isDeclaredInClass(mt.getIdentifier(), type.getJavaType()) ?
                Optional.of((Identifier<T, ?>) mt.getIdentifier()) : Optional.empty();
    }

    private static boolean isDeclaredInClass(FieldSpecification<?, ?> fs, Class<?> cls) {
        return fs.getJavaField().getDeclaringClass().equals(cls);
    }

    private static <T> Optional<FieldSpecification<T, ?>> getDeclaredAttribute(Field field, ManagedType<T> type) {
        return getDeclaredAttribute(field.getName(), type);
    }

    private static <T> Optional<FieldSpecification<T, ?>> getDeclaredAttribute(String fieldName, ManagedType<T> type) {
        try {
            return Optional.of(type.getDeclaredAttribute(fieldName));
        } catch (IllegalArgumentException e) {
            return Optional.empty();
        }
    }

    private static <T> Optional<FieldSpecification<T, ?>> getDeclaredTypes(Field field, ManagedType<T> type) {
        return type.getTypes() != null &&
                Objects.equals(field.getName(), type.getTypes().getJavaField().getName()) &&
                isDeclaredInClass(type.getTypes(), type.getJavaType()) ?
                Optional.of((TypesSpecification<T, ?>) type.getTypes()) : Optional.empty();
    }

    private static <T> Optional<FieldSpecification<T, ?>> getDeclaredProperties(Field field, ManagedType<T> type) {
        return type.getProperties() != null &&
                Objects.equals(field.getName(), type.getProperties().getJavaField().getName()) &&
                isDeclaredInClass(type.getProperties(), type.getJavaType()) ?
                Optional.of((PropertiesSpecification<T, ?, ?, ?>) type.getProperties()) : Optional.empty();
    }

    private void processManagedTypes() {
        final Set<ManagedType<?>> managedTypes = metamodel.getManagedTypes();
        managedTypes.removeAll(metamodel.getEntities());
        managedTypes.forEach(this::processType);
    }
}
