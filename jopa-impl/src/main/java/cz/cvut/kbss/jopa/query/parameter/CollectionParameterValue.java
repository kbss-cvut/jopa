package cz.cvut.kbss.jopa.query.parameter;

import java.util.List;
import java.util.stream.Collectors;

class CollectionParameterValue extends AbstractParameterValue {

    private final List<ParameterValue> values;

    CollectionParameterValue(List<ParameterValue> values) {
        this.values = values;
    }

    @Override
    public Object getValue() {
        return values.stream().map(ParameterValue::getValue).collect(Collectors.toList());
    }

    @Override
    public String getQueryString() {
        return values.stream().map(ParameterValue::getQueryString).collect(Collectors.joining(","));
    }
}
