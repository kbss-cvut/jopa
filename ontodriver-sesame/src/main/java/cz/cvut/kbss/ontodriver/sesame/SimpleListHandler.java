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
import cz.cvut.kbss.ontodriver_new.model.NamedResource;

class SimpleListHandler extends ListHandler<SimpleListDescriptor, SimpleListValueDescriptor> {

	SimpleListHandler(Connector connector, ValueFactory vf) {
		super(connector, vf);
	}

	@Override
	Collection<Axiom<?>> loadList(SimpleListDescriptor listDescriptor) throws SesameDriverException {
		final Collection<Axiom<?>> axioms = new ArrayList<>();
		final SimpleListIterator it = new SimpleListIterator(listDescriptor, connector, vf);
		while (it.hasNext()) {
			axioms.add(it.nextAxiom());
		}
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
		} else if (isOldListEmpty(owner(listValueDescriptor), hasList(listValueDescriptor),
				listValueDescriptor.getListProperty().isInferred(), context(listValueDescriptor))) {
			persistList(listValueDescriptor);
		} else {
			mergeList(listValueDescriptor);
		}
	}

	/**
	 * We are using this code instead of iterator.remove for performance
	 * reasons. The iterator has to reconnect the list for each removed node,
	 * which takes a lot of time.
	 */
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

	private void mergeList(SimpleListValueDescriptor listDescriptor)
			throws SesameDriverException {
		final SimpleListIterator it = new SimpleListIterator(listDescriptor, connector, vf);
		int i = 0;
		while (it.hasNext() && i < listDescriptor.getValues().size()) {
			final Resource node = it.next();
			final NamedResource newNode = listDescriptor.getValues().get(i);
			if (!node.stringValue().equals(newNode.getIdentifier().toString())) {
				it.replaceCurrentWith(newNode);
			}
			i++;
		}
		while (it.hasNext()) {
			it.next();
			it.remove();
		}
		assert i > 0;
		while (i < listDescriptor.getValues().size()) {
			final NamedResource previous = listDescriptor.getValues().get(i - 1);
			final NamedResource newNode = listDescriptor.getValues().get(i);
			final Statement stmt = vf.createStatement(sesameUri(previous.getIdentifier()),
					sesameUri(listDescriptor.getNextNode().getIdentifier()),
					sesameUri(newNode.getIdentifier()), context(listDescriptor));
			connector.addStatements(Collections.singleton(stmt));
			i++;
		}
	}
}
