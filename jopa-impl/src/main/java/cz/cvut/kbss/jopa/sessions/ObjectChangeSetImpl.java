package cz.cvut.kbss.jopa.sessions;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import cz.cvut.kbss.jopa.model.EntityDescriptor;

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

	// Set of changes
	private List<ChangeRecord> changes;

	// A map of attributeName-changeRecord pairs to easily find the attributes
	// to change
	private transient Map<String, ChangeRecord> attributesToChange;

	// Does this change set represent a new object
	private boolean isNew;

	// URI of the ontology context the object belongs to
	private EntityDescriptor repository;

	protected ObjectChangeSetImpl() {
	}

	public ObjectChangeSetImpl(Object changedObject, Object cloneObject, EntityDescriptor repository) {
		this.changedObject = changedObject;
		this.cloneObject = cloneObject;
		this.objectClass = cloneObject.getClass();
		if (repository == null) {
			throw new NullPointerException();
		}
		this.repository = repository;
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
		ChangeRecord existing = getAttributesToChange().get(attributeName);
		if (existing != null) {
			getChanges().remove(existing);
		}
		getChanges().add(record);
		getAttributesToChange().put(attributeName, record);
	}

	/**
	 * Returns the change records collections.
	 * 
	 * @return java.util.List<ChangeRecord>
	 */
	public List<ChangeRecord> getChanges() {
		if (this.changes == null) {
			this.changes = new ArrayList<ChangeRecord>();
		}
		return this.changes;
	}

	/**
	 * Returns the map with attribute names and changes made to them.
	 * 
	 * @return java.util.Map<String, ChangeRecord>
	 */
	public Map<String, ChangeRecord> getAttributesToChange() {
		if (this.attributesToChange == null) {
			this.attributesToChange = new HashMap<String, ChangeRecord>();
		}
		return this.attributesToChange;
	}

	public Class<?> getObjectClass() {
		return this.objectClass;
	}

	public Object getChangedObject() {
		return this.changedObject;
	}

	public Object getCloneObject() {
		return cloneObject;
	}

	public void setCloneObject(Object cloneObject) {
		this.cloneObject = cloneObject;
	}

	/**
	 * Set the change record list.
	 * 
	 * @param changes
	 *            List<ChangeRecord>
	 */
	public void setChanges(List<ChangeRecord> changes) {
		this.changes = changes;
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

	/**
	 * Retrieves the repository the referenced entity belongs to
	 */
	@Override
	public EntityDescriptor getEntityOrigin() {
		return repository;
	}
}
