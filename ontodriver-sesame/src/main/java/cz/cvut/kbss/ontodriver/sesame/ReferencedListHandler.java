package cz.cvut.kbss.ontodriver.sesame;

import cz.cvut.kbss.ontodriver.sesame.connector.Connector;
import cz.cvut.kbss.ontodriver.sesame.exceptions.SesameDriverException;
import cz.cvut.kbss.ontodriver_new.descriptors.ReferencedListDescriptor;
import cz.cvut.kbss.ontodriver_new.descriptors.ReferencedListValueDescriptor;
import cz.cvut.kbss.ontodriver_new.model.NamedResource;
import org.openrdf.model.Resource;
import org.openrdf.model.Statement;
import org.openrdf.model.URI;
import org.openrdf.model.ValueFactory;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;

public class ReferencedListHandler extends
		ListHandler<ReferencedListDescriptor, ReferencedListValueDescriptor> {

	private int sequenceCounter = 0;

	ReferencedListHandler(Connector connector, ValueFactory vf) {
		super(connector, vf);
	}

	@Override
	SesameIterator createIterator(ReferencedListDescriptor listDescriptor) throws SesameDriverException {
		return new ReferencedListIterator(listDescriptor, connector, vf);
	}

	URI createListHead(ReferencedListValueDescriptor listValueDescriptor,
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
		boolean unique;
		URI node;
		do {
			node = vf.createURI(uriBase + "-SEQ_" + sequenceCounter++);
			final Collection<Statement> stmts = connector.findStatements(node, null, null, false,
					context);
			unique = stmts.isEmpty();
		} while (!unique);
		return node;
	}

	Collection<Statement> createListRest(URI headNode,
			ReferencedListValueDescriptor listValueDescriptor) throws SesameDriverException {
		final URI owner = owner(listValueDescriptor);
		final URI hasNext = hasNext(listValueDescriptor);
		final URI hasContent = hasContent(listValueDescriptor);
		final URI context = context(listValueDescriptor);
		URI previous = headNode;
		final Collection<Statement> statements = new ArrayList<>(
				listValueDescriptor.getValues().size() * 2);
		final Iterator<NamedResource> it = listValueDescriptor.getValues().iterator();
		// Skip the first element, it is already in the head
		it.next();
		while (it.hasNext()) {
			final URI content = sesameUri(it.next().getIdentifier());
			previous = createListNode(owner, hasNext, hasContent, content, context, previous, statements);
		}
		return statements;
	}

	private URI createListNode(URI owner, URI hasNext, URI hasContent, URI content, URI context, Resource previous, Collection<Statement> statements) throws SesameDriverException {
		final URI node = generateSequenceNode(owner, context);
		statements.add(vf.createStatement(previous, hasNext, node, context));
		statements.add(vf.createStatement(node, hasContent, content, context));
		return node;
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
	SesameIterator iterator(ReferencedListValueDescriptor listDescriptor) throws SesameDriverException {
		return new ReferencedListIterator(listDescriptor, connector, vf);
	}

	@Override
	MergeResult mergeWithOriginalList(ReferencedListValueDescriptor listDescriptor, SesameIterator it) throws SesameDriverException {
		int i = 0;
		Resource node = null;
		while (it.hasNext() && i < listDescriptor.getValues().size()) {
			node = it.nextNode();
			final Resource content = it.currentContent();
			final NamedResource newNode = listDescriptor.getValues().get(i);
			if (!content.stringValue().equals(newNode.getIdentifier().toString())) {
				it.replaceCurrentWith(newNode);
			}
			i++;
		}
		return new MergeResult(i, node);
	}

	@Override
	void appendNewNodes(ReferencedListValueDescriptor listDescriptor, MergeResult mergeResult) throws SesameDriverException {
		int i = mergeResult.i;
		Resource previous = mergeResult.previous;
		final URI owner = owner(listDescriptor);
		final URI hasNext = hasNext(listDescriptor);
		final URI hasContent = hasContent(listDescriptor);
		final URI context = context(listDescriptor);
		assert i > 0;
		final Collection<Statement> toAdd = new ArrayList<>(
				(listDescriptor.getValues().size() - i) * 2);
		while (i < listDescriptor.getValues().size()) {
			final URI content = sesameUri(listDescriptor.getValues().get(i).getIdentifier());
			previous = createListNode(owner, hasNext, hasContent, content, context, previous, toAdd);
			i++;
		}
		connector.addStatements(toAdd);
	}
}
