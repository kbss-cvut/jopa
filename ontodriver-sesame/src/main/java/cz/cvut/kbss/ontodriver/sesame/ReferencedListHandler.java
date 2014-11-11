package cz.cvut.kbss.ontodriver.sesame;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;

import org.openrdf.model.Statement;
import org.openrdf.model.URI;
import org.openrdf.model.ValueFactory;

import cz.cvut.kbss.ontodriver.sesame.connector.Connector;
import cz.cvut.kbss.ontodriver.sesame.exceptions.SesameDriverException;
import cz.cvut.kbss.ontodriver_new.descriptors.ReferencedListDescriptor;
import cz.cvut.kbss.ontodriver_new.descriptors.ReferencedListValueDescriptor;
import cz.cvut.kbss.ontodriver_new.model.Axiom;
import cz.cvut.kbss.ontodriver_new.model.NamedResource;

public class ReferencedListHandler extends
		ListHandler<ReferencedListDescriptor, ReferencedListValueDescriptor> {

	private int sequenceCounter = 0;

	ReferencedListHandler(Connector connector, ValueFactory vf) {
		super(connector, vf);
	}

	@Override
	Collection<Axiom<?>> loadList(ReferencedListDescriptor listDescriptor)
			throws SesameDriverException {
		final List<Axiom<?>> axioms = new ArrayList<>();
		final SesameIterator it = new ReferencedListIterator(listDescriptor, connector, vf);
		while (it.hasNext()) {
			axioms.add(it.nextAxiom());
		}
		return axioms;
	}

	@Override
	void persistList(ReferencedListValueDescriptor listValueDescriptor)
			throws SesameDriverException {
		if (listValueDescriptor.getValues().isEmpty()) {
			return;
		}
		final Collection<Statement> statements = new ArrayList<>(listValueDescriptor.getValues()
				.size() * 2);
		URI head = createListHead(listValueDescriptor, statements);
		statements.addAll(createListRest(head, listValueDescriptor));
		connector.addStatements(statements);
	}

	private URI createListHead(ReferencedListValueDescriptor listValueDescriptor,
			Collection<Statement> statements) throws SesameDriverException {
		final URI owner = owner(listValueDescriptor);
		final URI hasList = hasList(listValueDescriptor);
		final URI hasContent = hasContent(listValueDescriptor);
		final URI context = context(listValueDescriptor);
		final URI nodeUri = generateSequenceNode(owner, context);
		statements.add(vf.createStatement(owner, hasList, nodeUri, context));
		final URI nodeContent = sesameUri(listValueDescriptor.getValues().get(0).getIdentifier());
		statements.add(vf.createStatement(nodeUri, hasContent, nodeContent, context));
		return nodeUri;
	}

	private URI hasContent(ReferencedListDescriptor listDescriptor) {
		return sesameUri(listDescriptor.getNodeContent().getIdentifier());
	}

	private URI generateSequenceNode(URI owner, URI context) throws SesameDriverException {
		final String uriBase = owner.stringValue();
		boolean unique = true;
		URI node = null;
		do {
			node = vf.createURI(uriBase + "-SEQ_" + sequenceCounter++);
			final Collection<Statement> stmts = connector.findStatements(node, null, null, false,
					context);
			unique = stmts.isEmpty();
		} while (!unique);
		return node;
	}

	private Collection<Statement> createListRest(URI headNode,
			ReferencedListValueDescriptor listValueDescriptor) throws SesameDriverException {
		final URI owner = owner(listValueDescriptor);
		final URI hasNext = hasNext(listValueDescriptor);
		final URI hasContent = hasContent(listValueDescriptor);
		final URI context = context(listValueDescriptor);
		URI previous = headNode;
		final Collection<Statement> stmts = new ArrayList<>(
				listValueDescriptor.getValues().size() * 2);
		final Iterator<NamedResource> it = listValueDescriptor.getValues().iterator();
		// Skip the first element, it is already in the head
		it.next();
		while (it.hasNext()) {
			final URI node = generateSequenceNode(owner, context);
			stmts.add(vf.createStatement(previous, hasNext, node, context));
			final URI content = sesameUri(it.next().getIdentifier());
			stmts.add(vf.createStatement(node, hasContent, content, context));
			previous = node;
		}
		return stmts;
	}

	@Override
	void clearList(ReferencedListValueDescriptor listDescriptor) throws SesameDriverException {
		// TODO Auto-generated method stub

	}

	@Override
	void mergeList(ReferencedListValueDescriptor listDescriptor) throws SesameDriverException {
		// TODO Auto-generated method stub

	}
}
