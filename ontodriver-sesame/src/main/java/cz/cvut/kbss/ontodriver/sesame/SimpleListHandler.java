package cz.cvut.kbss.ontodriver.sesame;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

import org.openrdf.model.Resource;
import org.openrdf.model.Statement;
import org.openrdf.model.URI;
import org.openrdf.model.ValueFactory;

import cz.cvut.kbss.ontodriver.sesame.connector.Connector;
import cz.cvut.kbss.ontodriver.sesame.exceptions.SesameDriverException;
import cz.cvut.kbss.ontodriver_new.descriptors.SimpleListDescriptor;
import cz.cvut.kbss.ontodriver_new.descriptors.SimpleListValueDescriptor;
import cz.cvut.kbss.ontodriver_new.model.Axiom;
import cz.cvut.kbss.ontodriver_new.model.AxiomImpl;
import cz.cvut.kbss.ontodriver_new.model.NamedResource;
import cz.cvut.kbss.ontodriver_new.model.Value;

class SimpleListHandler extends ListHandler<SimpleListDescriptor, SimpleListValueDescriptor> {

	private SimpleListDescriptor listDescriptor;

	SimpleListHandler(Connector connector, ValueFactory vf) {
		super(connector, vf);
	}

	@Override
	Collection<Axiom<?>> loadList(SimpleListDescriptor listDescriptor) throws SesameDriverException {
		final Collection<Axiom<?>> axioms = new ArrayList<>();
		this.listDescriptor = listDescriptor;
		// TODO Use the SimpleListIterator to load axioms
		final Axiom<java.net.URI> head = loadListHead();
		if (head == null) {
			return Collections.emptyList();
		}
		axioms.add(head);
		final Resource headElem = sesameUri(head.getValue().getValue());
		axioms.addAll(loadListRest(headElem));
		return axioms;
	}

	private Axiom<java.net.URI> loadListHead() throws SesameDriverException {
		final URI context = context(listDescriptor);
		final URI hasListProperty = hasList(listDescriptor);
		final URI owner = owner(listDescriptor);
		Collection<Statement> stmts = connector.findStatements(owner, hasListProperty, null,
				listDescriptor.getListProperty().isInferred(), context);
		if (stmts.isEmpty()) {
			return null;
		}
		final Resource head = extractListNode(stmts, listDescriptor.getListProperty());
		final java.net.URI javaHead = SesameUtils.toJavaUri(head);
		return createAxiom(javaHead);
	}

	private Axiom<java.net.URI> createAxiom(final java.net.URI nodeValue) {
		return new AxiomImpl<java.net.URI>(listDescriptor.getListOwner(),
				listDescriptor.getListProperty(), new Value<java.net.URI>(nodeValue));
	}

	private Collection<Axiom<?>> loadListRest(Resource firstElem) throws SesameDriverException {
		final Collection<Axiom<?>> axioms = new ArrayList<>();
		final URI context = context(listDescriptor);
		Resource subject = firstElem;
		final URI nextElemProperty = hasNext(listDescriptor);
		Collection<Statement> stmts = null;
		do {
			stmts = connector.findStatements(subject, nextElemProperty, null, listDescriptor
					.getNextNode().isInferred(), context);
			if (!stmts.isEmpty()) {
				final Resource listNode = extractListNode(stmts, listDescriptor.getNextNode());
				axioms.add(createAxiom(SesameUtils.toJavaUri(listNode)));
				subject = listNode;
			}
		} while (!stmts.isEmpty());
		return axioms;
	}

	@Override
	void persistList(SimpleListValueDescriptor listValueDescriptor) throws SesameDriverException {
		if (listValueDescriptor.getValues().isEmpty()) {
			return;
		}
		final Collection<Statement> stmts = new ArrayList<>();
		stmts.add(createListHead(listValueDescriptor));
		stmts.addAll(createListRest(listValueDescriptor));
		connector.addStatements(stmts);
	}

	private Statement createListHead(SimpleListValueDescriptor listValueDescriptor) {
		final Resource firstNode = sesameUri(listValueDescriptor.getValues().get(0).getIdentifier());
		return vf.createStatement(owner(listValueDescriptor), hasList(listValueDescriptor),
				firstNode, context(listValueDescriptor));
	}

	private List<Statement> createListRest(SimpleListValueDescriptor listValueDescriptor) {
		final List<Statement> stmts = new ArrayList<>(listValueDescriptor.getValues().size());
		NamedResource previous = listValueDescriptor.getValues().get(0);
		final URI nextNodeProp = hasNext(listValueDescriptor);
		final URI context = context(listValueDescriptor);
		for (NamedResource elem : listValueDescriptor.getValues()) {
			if (elem == previous) {
				continue;
			}
			final Resource subject = sesameUri(previous.getIdentifier());
			final Resource object = sesameUri(elem.getIdentifier());
			stmts.add(vf.createStatement(subject, nextNodeProp, object, context));
			previous = elem;
		}
		return stmts;
	}

	@Override
	void updateList(SimpleListValueDescriptor listValueDescriptor) throws SesameDriverException {
		if (listValueDescriptor.getValues().isEmpty()) {
			clearList(listValueDescriptor);
		}
		if (isOldListEmpty(owner(listValueDescriptor), hasList(listValueDescriptor),
				listValueDescriptor.getListProperty().isInferred(), context(listValueDescriptor))) {
			persistList(listValueDescriptor);
		}
		// TODO
	}

	private void clearList(SimpleListValueDescriptor listValueDescriptor)
			throws SesameDriverException {
		final URI owner = owner(listValueDescriptor);
		final URI hasList = hasList(listValueDescriptor);
		final URI context = context(listValueDescriptor);
		final Collection<Statement> toRemove = new ArrayList<>();
		Collection<Statement> stmts = connector.findStatements(owner, hasList, null,
				listValueDescriptor.getListProperty().isInferred(), context);
		if (stmts.isEmpty()) {
			return;
		}
		Resource subject = extractListNode(stmts, listValueDescriptor.getListProperty());
		toRemove.addAll(stmts);
		final URI hasNext = hasNext(listValueDescriptor);
		final boolean includeInferred = listValueDescriptor.getNextNode().isInferred();
		do {
			stmts = connector.findStatements(subject, hasNext, null, includeInferred, context);
			if (!stmts.isEmpty()) {
				subject = extractListNode(stmts, listValueDescriptor.getNextNode());
				toRemove.addAll(stmts);
			}
		} while (!stmts.isEmpty());
		connector.removeStatements(toRemove);
	}

	private boolean isOldListEmpty(Resource owner, URI hasListProperty, boolean includeInferred,
			URI context) throws SesameDriverException {
		final Collection<Statement> stmts = connector.findStatements(owner, hasListProperty, null,
				includeInferred, context);
		return stmts.isEmpty();
	}
}
