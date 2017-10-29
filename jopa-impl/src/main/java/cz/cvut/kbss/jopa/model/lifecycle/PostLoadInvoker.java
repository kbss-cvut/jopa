package cz.cvut.kbss.jopa.model.lifecycle;

import cz.cvut.kbss.jopa.model.MetamodelImpl;
import cz.cvut.kbss.jopa.model.metamodel.EntityTypeImpl;

import java.util.function.Consumer;

/**
 * Invokes post load entity listeners for the passed object.
 */
public class PostLoadInvoker implements Consumer<Object> {

    private final MetamodelImpl metamodel;

    public PostLoadInvoker(MetamodelImpl metamodel) {
        this.metamodel = metamodel;
    }

    @Override
    public void accept(Object o) {
        final EntityTypeImpl<?> et = metamodel.entity(o.getClass());
        et.getLifecycleListenerManager().invokePostLoadCallbacks(o);
    }
}
