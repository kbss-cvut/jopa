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
package cz.cvut.kbss.jopa.utils;

import cz.cvut.kbss.jopa.exceptions.OWLPersistenceException;
import cz.cvut.kbss.jopa.model.annotations.Transient;
import cz.cvut.kbss.jopa.model.metamodel.*;
import cz.cvut.kbss.ontodriver.exception.PrimaryKeyNotSetException;
import cz.cvut.kbss.ontodriver.exception.UnassignableIdentifierException;

import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.net.URI;
import java.util.*;

/**
 * Utility class for entity properties.
 */
public class EntityPropertiesUtils {

    /**
     * Private constructor
     */
    private EntityPropertiesUtils() {
        throw new AssertionError("I am not for instantiation.");
    }

    /**
     * Extracts primary key from the specified {@code entity} and returns it. </p>
     *
     * @param entity    The entity to extract primary key from
     * @param metamodel Metamodel
     * @return IRI of the entity or null if it is not set
     * @throws NullPointerException    If {@code entity} or {@code metamodel} is null
     * @throws OWLPersistenceException If {@code entity} is not an entity or if the identifier is of an unknown type
     */
    public static Object getPrimaryKey(Object entity, Metamodel metamodel) {
        Objects.requireNonNull(entity);
        Objects.requireNonNull(metamodel);

        Object fieldValue;
        final EntityType<?> type = metamodel.entity(entity.getClass());
        fieldValue = getFieldValue(type.getIdentifier().getJavaField(), entity);
        return fieldValue;
    }

    /**
     * Sets value of the specified field.
     *
     * @param field    Field to set value on
     * @param instance Target instance (may be null for static fields)
     * @param value    The value to set
     */
    public static void setFieldValue(Field field, Object instance, Object value) {
        Objects.requireNonNull(field);
        if (!field.isAccessible()) {
            field.setAccessible(true);
        }
        try {
            field.set(instance, value);
        } catch (IllegalAccessException e) {
            throw new OWLPersistenceException("Unable to set field value.", e);
        }
    }

    /**
     * Gets value of the specified field from the specified instance.
     *
     * @param field    Field to get value of
     * @param instance Instance that contains the field
     * @return Field value
     */
    public static Object getFieldValue(Field field, Object instance) {
        Objects.requireNonNull(field);
        if (!field.isAccessible()) {
            field.setAccessible(true);
        }
        try {
            return field.get(instance);
        } catch (IllegalAccessException e) {
            throw new OWLPersistenceException("Unable to extract field value.", e);
        }
    }

    /**
     * Gets value of the specified attribute.
     *
     * @param attribute Attribute to extract value of
     * @param instance  Instance from which value will be extracted
     * @return Attribute value
     */
    public static Object getAttributeValue(FieldSpecification<?, ?> attribute, Object instance) {
        Objects.requireNonNull(attribute);
        final Field field = attribute.getJavaField();
        return getFieldValue(field, instance);
    }

    /**
     * Extracts entity's primary key according to the specified entity type.
     *
     * @param entity Entity
     * @param et     Entity type
     * @return Primary key, possibly null
     */
    public static <T> URI getPrimaryKey(T entity, EntityType<?> et) {
        try {
            final Object id = getFieldValue(et.getIdentifier().getJavaField(), entity);
            if (id == null) {
                return null;
            }
            return getValueAsURI(id);
        } catch (IllegalArgumentException e) {
            throw new OWLPersistenceException("Unable to extract entity identifier.", e);
        }
    }

    /**
     * Sets the specified primary key on the specified entity.
     *
     * @param primaryKey The key to set
     * @param entity     Target entity
     * @param et         Entity type
     */
    public static <T> void setPrimaryKey(Object primaryKey, T entity, EntityType<T> et) {
        final Identifier id = et.getIdentifier();
        final Field idField = id.getJavaField();
        try {
            final Object assignablePk = IdentifierTransformer.transformToIdentifier(primaryKey, idField.getType());
            setFieldValue(idField, entity, assignablePk);
        } catch (IllegalArgumentException e) {
            throw new UnassignableIdentifierException(e);
        }
    }

    /**
     * Transforms the specified value to URI (if possible). </p>
     *
     * @param value The value to transform
     * @return {@code URI}
     * @throws NullPointerException     If {@code value} is {@code null}
     * @throws IllegalArgumentException If {@code value} cannot be transformed to URI
     */
    public static URI getValueAsURI(Object value) {
        Objects.requireNonNull(value, ErrorUtils.constructNPXMessage("value"));

        return IdentifierTransformer.valueAsUri(value);
    }

    /**
     * Gets all instance fields of the specified class, including inherited ones. </p>
     *
     * @param cls The class to search
     * @return List of declared fields
     */
    public static List<Field> getAllFields(Class<?> cls) {
        final List<Field> fields = new ArrayList<>();
        fields.addAll(Arrays.asList(cls.getDeclaredFields()));
        Class<?> tmp = cls.getSuperclass();
        while (tmp != null) {
            fields.addAll(Arrays.asList(tmp.getDeclaredFields()));
            tmp = tmp.getSuperclass();
        }
        Iterator<Field> it = fields.iterator();
        while (it.hasNext()) {
            Field f = it.next();
            if (Modifier.isStatic(f.getModifiers())) {
                it.remove();
            }
        }
        return fields;
    }

    /**
     * Verifies, that the primary key (identifier) of the specified instance is generated. </p>
     * <p>
     * If not, an exception is thrown.
     *
     * @param instance   The instance to verify
     * @param entityType Entity type of the instance, as specified by metamodel
     * @throws PrimaryKeyNotSetException If the identifier is not generated
     */
    public static void verifyIdentifierIsGenerated(Object instance, EntityType<?> entityType) {
        if (!entityType.getIdentifier().isGenerated()) {
            throw new PrimaryKeyNotSetException("The id for entity " + instance
                    + " is null and it is not specified as \'generated\' ");
        }
    }

    /**
     * Checks whether the specified field should not be persisted, i.e. whether it is transient in the persistence
     * sense.
     * <p>
     * A field is transient if it is:
     * <pre>
     * <ul>
     *     <li>static</li>
     *     <li>or final</li>
     *     <li>or transient</li>
     *     <li>or annotated with the {@link Transient} annotation</li>
     * </ul>
     * </pre>
     *
     * @param field The field to investigate
     * @return Whether the field is transient
     */
    public static boolean isFieldTransient(Field field) {
        Objects.requireNonNull(field);
        final int modifiers = field.getModifiers();
        if (Modifier.isStatic(modifiers) || Modifier.isFinal(modifiers) || Modifier.isTransient(modifiers)) {
            return true;
        }
        final Transient transientAnnotation = field.getAnnotation(Transient.class);
        return transientAnnotation != null;
    }
}
