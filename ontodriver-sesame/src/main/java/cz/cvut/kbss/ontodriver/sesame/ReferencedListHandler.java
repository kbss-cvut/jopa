package cz.cvut.kbss.ontodriver.sesame;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;

import org.openrdf.model.Resource;
import org.openrdf.model.Statement;
import org.openrdf.model.URI;
import org.openrdf.model.ValueFactory;

import cz.cvut.kbss.ontodriver.sesame.connector.Connector;
import cz.cvut.kbss.ontodriver.sesame.exceptions.SesameDriverException;
import cz.cvut.kbss.ontodriver_new.descriptors.ReferencedListDescriptor;
import cz.cvut.kbss.ontodriver_new.descriptors.ReferencedListValueDescriptor;
import cz.cvut.kbss.ontodriver_new.model.Axiom;
import cz.cvut.kbss.ontodriver_new.model.NamedResource;

// TODO Refactor list clear, persist and merge into separate classes, 
// which can share some logic for Simple and Referenced list, the procedure is almost the same
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
		final URI hasNext = hasNext(listDescriptor);
		final URI hasContent = hasContent(listDescriptor);
		final boolean includeInferred = listDescriptor.getListProperty().isInferred();
		final URI context = context(listDescriptor);
		Resource previous = owner(listDescriptor);
		URI currentProperty = hasList(listDescriptor);
		final Collection<Statement> toRemove = new ArrayList<>();
		Collection<Statement> next;
		do {
			next = connector.findStatements(previous, currentProperty, null, includeInferred,
					context);
			if (!next.isEmpty()) {
				final Resource node = extractListNode(next, currentProperty);
				toRemove.addAll(next);
				toRemove.addAll(connector.findStatements(node, hasContent, null, includeInferred,
						context));
				previous = node;
			}
			currentProperty = hasNext;
		} while (!next.isEmpty());
		connector.removeStatements(toRemove);
	}

	@Override
	void mergeList(ReferencedListValueDescriptor listDescriptor) throws SesameDriverException {
		final ReferencedListIterator it = new ReferencedListIterator(listDescriptor, connector, vf);
		Resource node = null;
		int i = 0;
		while (it.hasNext() && i < listDescriptor.getValues().size()) {
			node = it.nextNode();
			final Resource content = it.currentContent();
			final NamedResource newNode = listDescriptor.getValues().get(i);
			if (!content.stringValue().equals(newNode.getIdentifier().toString())) {
				it.replaceCurrentWith(newNode);
			}
			i++;
		}
		while (it.hasNext()) {
			it.nextNode();
			it.remove();
		}
		assert node != null;
		Resource previous = node;
		final URI owner = owner(listDescriptor);
		final URI hasNext = hasNext(listDescriptor);
		final URI hasContent = hasContent(listDescriptor);
		final URI context = context(listDescriptor);
		assert i > 0;
		final Collection<Statement> toAdd = new ArrayList<>(
				(listDescriptor.getValues().size() - i) * 2);
		while (i < listDescriptor.getValues().size()) {
			final Resource nextNode = generateSequenceNode(owner, context);
			final Statement nextStmt = vf.createStatement(previous, hasNext, nextNode, context);
			toAdd.add(nextStmt);
			final Statement nextContent = vf.createStatement(nextNode, hasContent,
					sesameUri(listDescriptor.getValues().get(i).getIdentifier()), context);
			toAdd.add(nextContent);
			previous = nextNode;
			i++;
		}
		connector.addStatements(toAdd);
	}
}
