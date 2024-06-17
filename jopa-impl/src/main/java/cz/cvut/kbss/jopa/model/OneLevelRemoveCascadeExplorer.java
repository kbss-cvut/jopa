package cz.cvut.kbss.jopa.model;

import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.proxy.lazy.LazyLoadingProxy;
import cz.cvut.kbss.jopa.utils.EntityPropertiesUtils;
import cz.cvut.kbss.jopa.utils.JOPALazyUtils;

import java.util.Collection;
import java.util.HashSet;
import java.util.function.Consumer;

public class OneLevelRemoveCascadeExplorer extends OneLevelCascadeExplorer {

    private final Consumer<Object> removeOperation;

    public OneLevelRemoveCascadeExplorer(Consumer<Object> removeOperation) {
        this.removeOperation = removeOperation;
    }

    @Override
    protected void exploreCascaded(Attribute<?, ?> at, Object o) {
        Object attVal = EntityPropertiesUtils.getAttributeValue(at, o);
        if (attVal == null) {
            return;
        }
        if (JOPALazyUtils.isLazyLoadingProxy(attVal)) {
            attVal = ((LazyLoadingProxy<?>) attVal).triggerLazyLoading();
        }
        if (at.isCollection()) {
            for (final Object ox2 : new HashSet<>((Collection<?>) attVal)) {
                removeOperation.accept(ox2);
            }
        } else {
            removeOperation.accept(attVal);
        }
    }
}
