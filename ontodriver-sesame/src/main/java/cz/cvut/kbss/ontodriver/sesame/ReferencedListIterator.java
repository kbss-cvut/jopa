package cz.cvut.kbss.ontodriver.sesame;

import java.util.Collection;

import org.openrdf.model.Resource;
import org.openrdf.model.Statement;
import org.openrdf.model.URI;
import org.openrdf.model.ValueFactory;

import cz.cvut.kbss.ontodriver.exceptions.IntegrityConstraintViolatedException;
import cz.cvut.kbss.ontodriver.sesame.connector.Connector;
import cz.cvut.kbss.ontodriver.sesame.exceptions.SesameDriverException;
import cz.cvut.kbss.ontodriver_new.descriptors.ReferencedListDescriptor;
import cz.cvut.kbss.ontodriver_new.model.Axiom;
import cz.cvut.kbss.ontodriver_new.model.NamedResource;

class ReferencedListIterator extends AbstractSesameIterator {

	private final ReferencedListDescriptor listDescriptor;

	private final URI hasContentProperty;

	private URI currentProperty;

	private Statement currentNode;
	private Statement currentContent;
	private Collection<Statement> next;

	public ReferencedListIterator(ReferencedListDescriptor listDescriptor, Connector connector,
			ValueFactory vf) throws SesameDriverException {
		super(listDescriptor, connector, vf);
		this.listDescriptor = listDescriptor;
		this.hasContentProperty = SesameUtils.toSesameUri(listDescriptor.getNodeContent()
				.getIdentifier(), vf);
		this.currentProperty = hasListProperty;
		init();
	}

	private void init() throws SesameDriverException {
		this.next = connector.findStatements(listOwner, hasListProperty, null, includeInferred,
				context);
	}

	@Override
	public boolean hasNext() throws SesameDriverException {
		return (!next.isEmpty());
	}

	@Override
	public Resource nextNode() throws SesameDriverException {
		nextInternal();
		return (Resource) currentNode.getObject();
	}

	private void nextInternal() throws SesameDriverException {
		if (!hasNext()) {
			throw new IllegalStateException();
		}
		checkSuccessorMax(next, currentProperty);
		this.currentNode = next.iterator().next();
		this.currentProperty = currentNode.getPredicate();
		checkNodeIsResource(currentNode);
		final Resource elem = (Resource) currentNode.getObject();
		this.currentContent = getNodeContent(elem);
		this.next = connector.findStatements(elem, hasNextProperty, null, includeInferred, context);
	}

	private Statement getNodeContent(Resource node) throws SesameDriverException {
		final Collection<Statement> elems = connector.findStatements(node, hasContentProperty,
				null, includeInferred, context);
		checkSuccessorMax(elems, hasContentProperty);
		if (elems.isEmpty()) {
			throw new IntegrityConstraintViolatedException("Node " + node + " has no content.");
		}
		final Statement elem = elems.iterator().next();
		checkNodeIsResource(elem);
		return elem;
	}

	@Override
	public Resource nextContent() throws SesameDriverException {
		nextInternal();
		return (Resource) currentContent.getObject();
	}

	@Override
	public Axiom<java.net.URI> nextAxiom() throws SesameDriverException {
		nextInternal();
		assert currentContent.getObject() instanceof Resource;

		return createAxiom(currentContent.getSubject(), listDescriptor.getNodeContent(),
				(Resource) currentContent.getObject());
	}

	@Override
	public void remove() throws SesameDriverException {
		// TODO Auto-generated method stub

	}

	@Override
	public void replaceCurrentWith(NamedResource newNode) throws SesameDriverException {
		// TODO Auto-generated method stub

	}

}
