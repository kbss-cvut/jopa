package cz.cvut.kbss.jopa.query.parameter;

abstract class AbstractParameterValue implements ParameterValue {

    @Override
    public String toString() {
        return getQueryString();
    }
}
