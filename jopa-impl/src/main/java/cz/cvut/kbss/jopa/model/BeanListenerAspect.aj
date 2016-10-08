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

import java.lang.reflect.Field;

import cz.cvut.kbss.jopa.exceptions.OWLPersistenceException;
import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import cz.cvut.kbss.jopa.model.annotations.OWLDataProperty;
import cz.cvut.kbss.jopa.model.annotations.OWLObjectProperty;
import cz.cvut.kbss.jopa.model.annotations.OWLAnnotationProperty;
import cz.cvut.kbss.jopa.model.annotations.Properties;
import cz.cvut.kbss.jopa.model.annotations.Types;
import cz.cvut.kbss.jopa.utils.EntityPropertiesUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public aspect BeanListenerAspect {

    private static final Logger LOG = LoggerFactory.getLogger(BeanListenerAspect.class);

    pointcut getter(): get( @(OWLObjectProperty || OWLDataProperty || OWLAnnotationProperty || Types || Properties ) * * ) && within(@OWLClass *);

    pointcut setter(): set( @(OWLObjectProperty || OWLDataProperty || OWLAnnotationProperty || Types || Properties ) * * ) && within(@OWLClass *);

    before(): setter() {
        // Check for inferred field modification
        final Object object = thisJoinPoint.getTarget();
        Class<?> cls = object.getClass();
        Field field = null;
        final String fieldName = thisJoinPoint.getSignature().getName();
        try {
            field = cls.getDeclaredField(fieldName);
        } catch (NoSuchFieldException e) {
            Class<?> superCls = cls;
            while ((superCls = superCls.getSuperclass()) != null) {
                try {
                    field = superCls.getDeclaredField(fieldName);
                    break;
                } catch (NoSuchFieldException ex) {
                    // Do nothing, keep trying
                }
            }
            if (field == null) {
                throw new OWLPersistenceException(e.getMessage());
            }
        } catch (SecurityException e) {
            LOG.error(e.getMessage(), e);
            throw new OWLPersistenceException(e.getMessage());
        }
        JOPAPersistenceProvider.verifyInferredAttributeNotModified(object, field);
    }

    after() returning : setter() {
        // Persist changes done during transaction
        final Object entity = thisJoinPoint.getTarget();
        Field f;
        try {
            f = entity.getClass().getDeclaredField(thisJoinPoint.getSignature().getName());
            if (EntityPropertiesUtils.isFieldTransient(f)) {
                return;
            }
            JOPAPersistenceProvider.persistEntityChanges(entity, f);
        } catch (NoSuchFieldException | SecurityException e) {
            LOG.error(e.getMessage(), e);
            throw new OWLPersistenceException(e.getMessage());
        }
    }

    before(): getter()  {
        // Load lazy loaded entity field
        try {
            final Object object = thisJoinPoint.getTarget();
            final Field field = object.getClass().getDeclaredField(
                    thisJoinPoint.getSignature().getName());
            if (EntityPropertiesUtils.isFieldTransient(field)) {
                return;
            }

            field.setAccessible(true);

            LOG.trace("*** Fetching {} of {}: {}", field.getName(), object.getClass(), System.identityHashCode(object));

            JOPAPersistenceProvider.loadReference(object, field);
        } catch (NoSuchFieldException | IllegalArgumentException | IllegalAccessException e) {
            LOG.error(e.getMessage(), e);
            throw new OWLPersistenceException();
        }
    }
}
