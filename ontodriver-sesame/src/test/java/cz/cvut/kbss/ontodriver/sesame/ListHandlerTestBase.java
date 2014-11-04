package cz.cvut.kbss.ontodriver.sesame;

import java.util.ArrayList;
import java.util.List;

import org.openrdf.model.Resource;
import org.openrdf.model.URI;
import org.openrdf.model.ValueFactory;
import org.openrdf.repository.Repository;
import org.openrdf.repository.RepositoryException;
import org.openrdf.repository.sail.SailRepository;
import org.openrdf.sail.memory.MemoryStore;

import cz.cvut.kbss.ontodriver_new.model.NamedResource;

class ListHandlerTestBase {

	protected static final NamedResource OWNER = NamedResource
			.create("http://krizik.felk.cvut.cz/ontologies/jopa/entityC");
	protected static final String LIST_PROPERTY = "http://krizik.felk.cvut.cz/ontologies/jopa/attributes#C-hasSimpleSequence";
	protected static final String NEXT_NODE_PROPERTY = "http://krizik.felk.cvut.cz/ontologies/jopa/attributes#C-hasSimpleNext";

	protected static ValueFactory vf;
	protected static Repository repo;
	protected static Resource owner;
	protected static URI hasListProperty;
	protected static URI nextNodeProperty;

	protected static void init() {
		final MemoryStore mStore = new MemoryStore();
		repo = new SailRepository(mStore);
		vf = repo.getValueFactory();
		owner = vf.createURI(OWNER.toString());
		hasListProperty = vf.createURI(LIST_PROPERTY);
		nextNodeProperty = vf.createURI(NEXT_NODE_PROPERTY);
	}

	protected static void close() throws RepositoryException {
		repo.shutDown();
	}

	protected List<java.net.URI> initList() {
		final List<java.net.URI> lst = new ArrayList<>();
		for (int i = 0; i < 5; i++) {
			lst.add(java.net.URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/elem" + i));
		}
		return lst;
	}
}
