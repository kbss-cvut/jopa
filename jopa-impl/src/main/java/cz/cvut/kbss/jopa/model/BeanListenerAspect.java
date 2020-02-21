/**
 * Copyright (C) 2020 Czech Technical University in Prague
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
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.jopa.sessions.UnitOfWorkImpl;
import cz.cvut.kbss.jopa.sessions.validator.AttributeModificationValidator;
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

    @DeclareMixin(value = "!is(InterfaceType) && (@cz.cvut.kbss.jopa.model.annotations.OWLClass *)")
    public static Manageable createImpl() {
        return new ManageableImpl();
    }

    @Pointcut("get( @(cz.cvut.kbss.jopa.model.annotations.OWLObjectProperty " +
            "|| cz.cvut.kbss.jopa.model.annotations.OWLDataProperty " +
            "|| cz.cvut.kbss.jopa.model.annotations.OWLAnnotationProperty " +
            "|| cz.cvut.kbss.jopa.model.annotations.Types " +
            "|| cz.cvut.kbss.jopa.model.annotations.Properties ) * * ) " +
            "&& (within(@cz.cvut.kbss.jopa.model.annotations.OWLClass *) || within(@cz.cvut.kbss.jopa.model.annotations.MappedSuperclass *))")
    void getter() {
    }

    @Pointcut("set( @(cz.cvut.kbss.jopa.model.annotations.OWLObjectProperty " +
            "|| cz.cvut.kbss.jopa.model.annotations.OWLDataProperty " +
            "|| cz.cvut.kbss.jopa.model.annotations.OWLAnnotationProperty " +
            "|| cz.cvut.kbss.jopa.model.annotations.Types || cz.cvut.kbss.jopa.model.annotations.Properties ) * * ) " +
            "&& (within(@cz.cvut.kbss.jopa.model.annotations.OWLClass *) || within(@cz.cvut.kbss.jopa.model.annotations.MappedSuperclass *))")
    void setter() {
    }

    /**
     * Ties the specified instance to its persistence context, so that the advices can identify it easily.
     *
     * @param instance           The managed instance
     * @param persistenceContext Persistence context which is managing it
     */
    public void register(Object instance, UnitOfWorkImpl persistenceContext) {
        ((Manageable) instance).setPersistenceContext(persistenceContext);
    }

    /**
     * Disconnects the specified instance from its persistence context.
     *
     * @param instance The instance to remove
     * @see #register(Object, UnitOfWorkImpl)
     */
    public void deregister(Object instance) {
        ((Manageable) instance).setPersistenceContext(null);
    }

    @AfterReturning("setter()")
    public void afterSetter(JoinPoint thisJoinPoint) {
        // Persist changes done during transaction and check for inferred attribute modification
        final Object entity = thisJoinPoint.getTarget();
        if (!(entity instanceof Manageable)) {
            return;
        }
        final UnitOfWorkImpl persistenceContext = ((Manageable) entity).getPersistenceContext();
        if (persistenceContext == null || !persistenceContext.isInTransaction()) {
            return;
        }

        try {
            final FieldSpecification<?, ?> fieldSpec = getFieldSpecification(entity,
                    thisJoinPoint.getSignature().getName(), persistenceContext);
            AttributeModificationValidator.verifyCanModify(fieldSpec);
            persistenceContext.attributeChanged(entity, fieldSpec.getJavaField());
        } catch (SecurityException e) {
            LOG.error(e.getMessage(), e);
            throw new OWLPersistenceException(e.getMessage());
        }
    }

    private FieldSpecification<?, ?> getFieldSpecification(Object entity, String fieldName,
                                                           UnitOfWorkImpl persistenceContext) {
        final EntityType<?> et = persistenceContext.getMetamodel().entity(entity.getClass());
        assert et != null;
        return et.getFieldSpecification(fieldName);
    }

    @Before("getter()")
    public void beforeGetter(JoinPoint thisJoinPoint) {
        // Load lazy loaded entity field
        final Object entity = thisJoinPoint.getTarget();
        if (!(entity instanceof Manageable)) {
            return;
        }
        final UnitOfWorkImpl persistenceContext = ((Manageable) entity).getPersistenceContext();
        if (persistenceContext == null || !persistenceContext.contains(entity)) {
            return;
        }
        final FieldSpecification<?, ?> fieldSpec = getFieldSpecification(entity, thisJoinPoint.getSignature().getName(),
                persistenceContext);
        final Field field = fieldSpec.getJavaField();
        if (EntityPropertiesUtils.isFieldTransient(field)) {
            return;
        }
        persistenceContext.loadEntityField(entity, field);
    }
}
