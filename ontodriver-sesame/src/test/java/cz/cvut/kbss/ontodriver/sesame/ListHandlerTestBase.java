package cz.cvut.kbss.ontodriver.sesame;

import cz.cvut.kbss.ontodriver.model.NamedResource;
import org.openrdf.model.Resource;
import org.openrdf.model.ValueFactory;
import org.openrdf.repository.Repository;
import org.openrdf.repository.RepositoryException;
import org.openrdf.repository.sail.SailRepository;
import org.openrdf.sail.memory.MemoryStore;

import java.util.ArrayList;
import java.util.List;

class ListHandlerTestBase {

	protected static final NamedResource OWNER = NamedResource
			.create("http://krizik.felk.cvut.cz/ontologies/jopa/entityC");

	protected static final String LIST_PROPERTY = "http://krizik.felk.cvut.cz/ontologies/jopa/attributes#C-hasSequence";
	protected static final String NEXT_NODE_PROPERTY = "http://krizik.felk.cvut.cz/ontologies/jopa/attributes#C-hasNext";


	protected static ValueFactory vf;
	protected static Repository repo;
	protected static Resource owner;

	protected static void init() throws Exception {
		final MemoryStore mStore = new MemoryStore();
		repo = new SailRepository(mStore);
		repo.initialize();
		vf = repo.getValueFactory();
		owner = vf.createURI(OWNER.toString());
	}

	protected static void close() throws RepositoryException {
		repo.shutDown();
	}

	protected List<NamedResource> initList() {
		final List<NamedResource> lst = new ArrayList<>();
		for (int i = 0; i < 5; i++) {
			lst.add(NamedResource.create("http://krizik.felk.cvut.cz/ontologies/jopa/elem" + i));
		}
		return lst;
	}
}
