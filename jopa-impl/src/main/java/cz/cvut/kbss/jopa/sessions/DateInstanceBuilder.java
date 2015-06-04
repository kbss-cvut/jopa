package cz.cvut.kbss.jopa.sessions;

import cz.cvut.kbss.jopa.model.descriptors.Descriptor;

import java.lang.reflect.Field;
import java.util.Date;

/**
 * @author ledvima1
 */
public class DateInstanceBuilder extends AbstractInstanceBuilder {

    public DateInstanceBuilder(CloneBuilderImpl builder, UnitOfWork uow) {
        super(builder, uow);
    }

    @Override
    Object buildClone(Object cloneOwner, Field field, Object original, Descriptor descriptor) {
        if (original == null) {
            return null;
        }
        final Date orig = (Date) original;
        return new Date(orig.getTime());
    }

    @Override
    void mergeChanges(Field field, Object target, Object originalValue, Object cloneValue) throws
            IllegalArgumentException, IllegalAccessException {
        if (!field.isAccessible()) {
            field.setAccessible(true);
        }
        field.set(target, cloneValue);
    }

    @Override
    boolean populatesAttributes() {
        return true;
    }
}
