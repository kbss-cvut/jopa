package cz.cvut.kbss.jopa.sessions;

import java.net.URI;
import java.util.List;
import java.util.Map;

public interface ObjectChangeSet {

	public void addChangeRecord(ChangeRecord record);
	
	public Class<?> getObjectClass();
	
	public List<ChangeRecord> getChanges();
	
	public Map<String, ChangeRecord> getAttributesToChange();
	
	public boolean isNew();

	void setChanges(List<ChangeRecord> changes);

	public Object getCloneObject();
	
	public Object getChangedObject();
	
	public URI getEntityContext();
}
