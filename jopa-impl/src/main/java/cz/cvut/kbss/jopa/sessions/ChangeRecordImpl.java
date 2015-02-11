package cz.cvut.kbss.jopa.sessions;

public class ChangeRecordImpl implements ChangeRecord {

    private final String attributeName;

    private final Object newValue;

    public ChangeRecordImpl(String attributeName, Object attValue) {
        assert attributeName != null;
        this.attributeName = attributeName;
        this.newValue = attValue;
    }

    public Object getNewValue() {
        return this.newValue;
    }

    public String getAttributeName() {
        return this.attributeName;
    }

}
