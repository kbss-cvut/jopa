package cz.cvut.kbss.jopa.sessions;

import cz.cvut.kbss.jopa.sessions.ChangeRecord;

public class ChangeRecordImpl implements ChangeRecord {
	
	protected String attributeName;
	
	protected Object newValue;
	
	public ChangeRecordImpl(String attributeName, Object attValue) {
		this.attributeName = attributeName;
		this.newValue = attValue;
	}

	public Object getNewValue() {
		return this.newValue;
	}

	public void setNewValue(Object newValue) {
		this.newValue = newValue;

	}

	public String getAttributeName() {
		return this.attributeName;
	}

}
