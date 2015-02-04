package cz.cvut.kbss.jopa.sessions;

import java.io.Serializable;
import java.net.URI;
import java.util.HashMap;
import java.util.Map;

public class ObjectChangeSetImpl implements Serializable, ObjectChangeSet {

	/**
	 * 
	 */
	private static final long serialVersionUID = -1316908575232588565L;

	// Class this ObjectChangeSet represents
	private transient Class<?> objectClass;

	// The object the changes are bound to
	private Object changedObject;

	// Reference to the clone
	private transient Object cloneObject;

	// A map of attributeName-changeRecord pairs to easily find the attributes
	// to change
	private transient Map<String, ChangeRecord> attributesToChange = new HashMap<>();

	// Does this change set represent a new object
	private boolean isNew;

	private URI context;

	public ObjectChangeSetImpl(Object changedObject, Object cloneObject, URI context) {
		this.changedObject = changedObject;
		this.cloneObject = cloneObject;
		this.objectClass = cloneObject.getClass();
		this.context = context;
	}

	/**
	 * Add a change record to this change set
	 * 
	 * @param record
	 *            ChangeRecord
	 */
	public void addChangeRecord(ChangeRecord record) {
		if (record == null)
			return;
		String attributeName = record.getAttributeName();
		attributesToChange.put(attributeName, record);
	}

	/**
	 * Returns the map with attribute names and changes made to them.
	 * 
	 * @return java.util.Map
	 */
	public Map<String, ChangeRecord> getChanges() {
		return this.attributesToChange;
	}

	public Class<?> getObjectClass() {
		return objectClass;
	}

	public Object getChangedObject() {
		return changedObject;
	}

	public Object getCloneObject() {
		return cloneObject;
	}

	public void setCloneObject(Object cloneObject) {
		this.cloneObject = cloneObject;
	}

	public void setNew(boolean isNew) {
		this.isNew = isNew;
	}

	/**
	 * Returns true if this change set represents a new object
	 * 
	 * @return boolean
	 */
	public boolean isNew() {
		return isNew;
	}

	@Override
	public URI getEntityContext() {
		return context;
	}
}
