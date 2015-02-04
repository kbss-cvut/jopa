package cz.cvut.kbss.jopa.sessions;

import java.net.URI;
import java.util.Map;

public interface ObjectChangeSet {

    /**
     * Adds a new change record to this change set.
     * <p/>
     * If there was a change for attribute represented by the new record, it will be overwritten.
     *
     * @param record The record to add
     */
    public void addChangeRecord(ChangeRecord record);

    /**
     * Gets type of the changed object.
     *
     * @return Object type
     */
    public Class<?> getObjectClass();

    /**
     * Gets changes mapped by attribute names.
     *
     * @return Map of changes keyed by attribute names
     */
    public Map<String, ChangeRecord> getChanges();

    /**
     * Specifies whether this change set represents a new object.
     *
     * @param isNew Whether this is a new object's change set
     */
    public void setNew(boolean isNew);

    /**
     * Whether this is a new object's change set
     */
    public boolean isNew();

    /**
     * Gets the clone with changes.
     *
     * @return Clone
     */
    public Object getCloneObject();

    /**
     * Gets the original object.
     *
     * @return Original
     */
    public Object getChangedObject();

    /**
     * Gets ontology context URI, to which the changed object belongs.
     *
     * @return context URI
     */
    public URI getEntityContext();
}
