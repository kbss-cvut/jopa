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

import cz.cvut.kbss.jopa.exceptions.OWLInferredAttributeModifiedException;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.jopa.sessions.CloneBuilderImpl;
import cz.cvut.kbss.jopa.sessions.ServerSession;
import cz.cvut.kbss.jopa.sessions.UnitOfWorkImpl;
import cz.cvut.kbss.jopa.utils.EntityPropertiesUtils;

import java.lang.reflect.Field;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

public class JOPAPersistenceProvider implements PersistenceProvider, ProviderUtil {

    private static Set<EntityManagerFactoryImpl> emfs = new HashSet<>();

    public JOPAPersistenceProvider() {
    }

    public EntityManagerFactoryImpl createEntityManagerFactory(String emName, Map<String, String> properties) {
        final EntityManagerFactoryImpl emf = new EntityManagerFactoryImpl(properties);
        emfs.add(emf);
        return emf;
    }

    @Override
    public ProviderUtil getProviderUtil() {
        return this;
    }

    @Override
    public LoadState isLoaded(Object entity) {
        return LoadState.UNKNOWN;
    }

    @Override
    public LoadState isLoadedWithReference(Object entity, String attributeName) {
        return LoadState.UNKNOWN;
    }

    @Override
    public LoadState isLoadedWithoutReference(Object entity, String attributeName) {
        return LoadState.UNKNOWN;
    }

    private static UnitOfWorkImpl getPersistenceContext(Object entity) {
        if (entity == null) {
            return null;
        }
        for (EntityManagerFactoryImpl emf : emfs) {
            final ServerSession session = emf.getServerSession();
            if (session == null) {
                continue;
            }
            final UnitOfWorkImpl uow = session.getPersistenceContext(entity);
            if (uow != null) {
                return uow;
            }
        }
        return null;
    }

    static void loadReference(Object o, Field f) throws IllegalAccessException {
        final UnitOfWorkImpl uow = getPersistenceContext(o);

        if (uow != null) {
            Object managedOrig = uow.getOriginal(o);
            if (managedOrig == null) {
                return;
            }
            Object val = EntityPropertiesUtils.getFieldValue(f, managedOrig);
            if (val != null) {
                return;
            }
            uow.loadEntityField(o, f);
        }
    }

    /**
     * Write changes to the specified entity to the transaction ontology.
     *
     * @param entity Entity to persist
     */
    static void persistEntityChanges(Object entity, Field f) {
        final UnitOfWorkImpl uow = getPersistenceContext(entity);
        if (uow != null && uow.isInTransaction()) {
            uow.attributeChanged(entity, f);
        }
    }

    static void verifyInferredAttributeNotModified(Object entity, Field field) {
        if (getPersistenceContext(entity) == null) {
            return;
        }
        if (CloneBuilderImpl.isFieldInferred(field)) {
            throw new OWLInferredAttributeModifiedException("Modifying inferred attributes is forbidden.");
        }
    }

    /**
     * Gets field matching the specified name, declared in the specified managed entity or any of its managed supertypes.
     * <p>
     * The entity has to be managed in one of the currently open persistence contexts.
     *
     * @param entity    Entity instance
     * @param fieldName Name of the field to get
     * @return Matching field or {@code null}, if the entity is not managed or if no field with the specified name exists in the entity
     */
    static Field getEntityField(Object entity, String fieldName) {
        final UnitOfWorkImpl uow = getPersistenceContext(entity);
        if (uow == null) {
            return null;
        }
        final EntityType<?> et = uow.getMetamodel().entity(entity.getClass());
        assert et != null;  // If the instance is managed, it has to be an existing entity type
        final FieldSpecification<?, ?> fieldSpec = et.getFieldSpecification(fieldName);
        return fieldSpec != null ? fieldSpec.getJavaField() : null;
    }
}
