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
import cz.cvut.kbss.jopa.sessions.UnitOfWorkImpl;
import cz.cvut.kbss.jopa.utils.EntityPropertiesUtils;
import org.aspectj.lang.JoinPoint;
import org.aspectj.lang.annotation.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.lang.reflect.Field;

@Aspect
public class BeanListenerAspect {

    private static final Logger LOG = LoggerFactory.getLogger(BeanListenerAspect.class);

    public interface Manageable {
        void setPersistenceContext(UnitOfWorkImpl uow);

        UnitOfWorkImpl getPersistenceContext();
    }

    public static class ManageableImpl implements Manageable {
        private transient UnitOfWorkImpl persistenceContext;

        @Override
        public void setPersistenceContext(UnitOfWorkImpl uow) {
            this.persistenceContext = uow;
        }

        @Override
        public UnitOfWorkImpl getPersistenceContext() {
            return persistenceContext;
        }
    }

    @DeclareMixin(value = "(@cz.cvut.kbss.jopa.model.annotations.OWLClass *)")
    public static Manageable createImpl() {
        return new ManageableImpl();
    }

    @Pointcut(
            "get( @(cz.cvut.kbss.jopa.model.annotations.OWLObjectProperty || cz.cvut.kbss.jopa.model.annotations.OWLDataProperty || cz.cvut.kbss.jopa.model.annotations.OWLAnnotationProperty || cz.cvut.kbss.jopa.model.annotations.Types || cz.cvut.kbss.jopa.model.annotations.Properties ) * * ) && within(@cz.cvut.kbss.jopa.model.annotations.OWLClass *)")
    void getter() {
    }

    @Pointcut(
            "set( @(cz.cvut.kbss.jopa.model.annotations.OWLObjectProperty || cz.cvut.kbss.jopa.model.annotations.OWLDataProperty || cz.cvut.kbss.jopa.model.annotations.OWLAnnotationProperty || cz.cvut.kbss.jopa.model.annotations.Types || cz.cvut.kbss.jopa.model.annotations.Properties ) * * ) && within(@cz.cvut.kbss.jopa.model.annotations.OWLClass *)")
    void setter() {
    }


    public void register(Object instance, UnitOfWorkImpl persistenceContext) {
        LOG.info("Registering instance in PC.");
        ((Manageable) instance).setPersistenceContext(persistenceContext);
    }

    public void deregister(Object instance) {
        ((Manageable) instance).setPersistenceContext(null);
    }

    @AfterReturning("setter()")
    public void afterSetter(JoinPoint thisJoinPoint) {
        // Persist changes done during transaction and check for inferred attribute modification
        final Object entity = thisJoinPoint.getTarget();
        if (((Manageable) entity).getPersistenceContext() == null) {
            return;
        }

        final Field field;
        try {
            field = JOPAPersistenceProvider.getEntityField(entity, thisJoinPoint.getSignature().getName());
            if (field == null || EntityPropertiesUtils.isFieldTransient(field)) {
                return;
            }
            JOPAPersistenceProvider.verifyInferredAttributeNotModified(entity, field);
            JOPAPersistenceProvider.persistEntityChanges(entity, field);
        } catch (SecurityException e) {
            LOG.error(e.getMessage(), e);
            throw new OWLPersistenceException(e.getMessage());
        }
    }

    @Before("getter()")
    public void beforeGetter(JoinPoint thisJoinPoint) {
        // Load lazy loaded entity field
        try {
            final Object object = thisJoinPoint.getTarget();
            if (((Manageable) object).getPersistenceContext() == null) {
                return;
            }
            final Field field = JOPAPersistenceProvider.getEntityField(object, thisJoinPoint.getSignature().getName());
            if (field == null || EntityPropertiesUtils.isFieldTransient(field)) {
                return;
            }

            JOPAPersistenceProvider.loadReference(object, field);
        } catch (IllegalArgumentException | IllegalAccessException e) {
            LOG.error(e.getMessage(), e);
            throw new OWLPersistenceException(e);
        }
    }
}
