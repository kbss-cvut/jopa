package cz.cvut.kbss.jopa.query.parameter;

import cz.cvut.kbss.jopa.sessions.MetamodelProvider;
import cz.cvut.kbss.jopa.utils.EntityPropertiesUtils;

import java.util.Objects;

public class EntityParameterValue extends AbstractParameterValue {

    private final MetamodelProvider metamodelProvider;

    private final Object value;

    public EntityParameterValue(Object value, MetamodelProvider metamodelProvider) {
        this.value = Objects.requireNonNull(value);
        this.metamodelProvider = metamodelProvider;
        assert metamodelProvider.isEntityType(value.getClass());
    }

    @Override
    public Object getValue() {
        return value;
    }

    @Override
    public String getQueryString() {
        return "<" + EntityPropertiesUtils.getIdentifier(value, metamodelProvider.getMetamodel()) + ">";
    }
}
