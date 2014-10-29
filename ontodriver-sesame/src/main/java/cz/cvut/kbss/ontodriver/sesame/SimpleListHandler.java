package cz.cvut.kbss.ontodriver.sesame;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

import org.openrdf.model.Resource;
import org.openrdf.model.Statement;
import org.openrdf.model.URI;
import org.openrdf.model.Value;
import org.openrdf.model.ValueFactory;

import cz.cvut.kbss.ontodriver.exceptions.IntegrityConstraintViolatedException;
import cz.cvut.kbss.ontodriver.sesame.connector.Connector;
import cz.cvut.kbss.ontodriver.sesame.exceptions.SesameDriverException;
import cz.cvut.kbss.ontodriver_new.descriptors.SimpleListDescriptor;
import cz.cvut.kbss.ontodriver_new.descriptors.SimpleListValueDescriptor;
import cz.cvut.kbss.ontodriver_new.model.Axiom;
import cz.cvut.kbss.ontodriver_new.model.AxiomImpl;
import cz.cvut.kbss.ontodriver_new.model.NamedResource;

class SimpleListHandler extends ListHandler<SimpleListDescriptor, SimpleListValueDescriptor> {

	private SimpleListDescriptor listDescriptor;

	SimpleListHandler(Connector connector, ValueFactory vf) {
		super(connector, vf);
	}

	@Override
	Collection<Axiom<?>> loadList(SimpleListDescriptor listDescriptor) throws SesameDriverException {
		final Collection<Axiom<?>> axioms = new ArrayList<>();
		this.listDescriptor = listDescriptor;
		final Axiom<java.net.URI> head = loadListHead();
		if (head == null) {
			return Collections.emptyList();
		}
		axioms.add(head);
		final Resource headElem = SesameUtils.toSesameUri(head.getValue().getValue(), vf);
		axioms.addAll(loadListRest(headElem));
		return axioms;
	}

	private Axiom<java.net.URI> loadListHead() throws SesameDriverException {
		final URI context = SesameUtils.toSesameUri(listDescriptor.getContext(), vf);
		final URI hasListProperty = SesameUtils.toSesameUri(listDescriptor.getListProperty()
				.getIdentifier(), vf);
		final URI owner = SesameUtils
				.toSesameUri(listDescriptor.getListOwner().getIdentifier(), vf);
		Collection<Statement> stmts = connector.findStatements(owner, hasListProperty, null,
				listDescriptor.getListProperty().isInferred(), context);
		if (stmts.isEmpty()) {
			return null;
		}
		final Resource head = getListNode(stmts);
		final java.net.URI javaHead = SesameUtils.toJavaUri(head);
		return createAxiom(javaHead);
	}

	private Resource getListNode(Collection<Statement> stmts) throws SesameDriverException {
		if (stmts.size() > 1) {
			throw new IntegrityConstraintViolatedException(
					"Invalid number of values found for assertion "
							+ listDescriptor.getListProperty() + ". Expected 1, got "
							+ stmts.size());
		}
		final Value val = stmts.iterator().next().getObject();
		if (!(val instanceof Resource)) {
			throw new IntegrityConstraintViolatedException(
					"Invalid property value. Expected object property value, got literal.");
		}
		return (Resource) val;
	}

	private Axiom<java.net.URI> createAxiom(final java.net.URI nodeValue) {
		return new AxiomImpl<java.net.URI>(listDescriptor.getListOwner(),
				listDescriptor.getListProperty(),
				new cz.cvut.kbss.ontodriver_new.model.Value<java.net.URI>(nodeValue));
	}

	private Collection<Axiom<?>> loadListRest(Resource firstElem) throws SesameDriverException {
		final Collection<Axiom<?>> axioms = new ArrayList<>();
		final URI context = SesameUtils.toSesameUri(listDescriptor.getContext(), vf);
		Resource subject = firstElem;
		final URI nextElemProperty = SesameUtils.toSesameUri(listDescriptor.getNextNode()
				.getIdentifier(), vf);
		Collection<Statement> stmts = null;
		do {
			stmts = connector.findStatements(subject, nextElemProperty, null, listDescriptor
					.getNextNode().isInferred(), context);
			if (!stmts.isEmpty()) {
				final Resource listNode = getListNode(stmts);
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
		final Resource owner = SesameUtils.toSesameUri(listValueDescriptor.getListOwner()
				.getIdentifier(), vf);
		final URI listProp = SesameUtils.toSesameUri(listValueDescriptor.getListProperty()
				.getIdentifier(), vf);
		final Resource firstNode = SesameUtils.toSesameUri(listValueDescriptor.getValues().get(0)
				.getIdentifier(), vf);
		return vf.createStatement(owner, listProp, firstNode,
				SesameUtils.toSesameUri(listValueDescriptor.getContext(), vf));
	}

	private List<Statement> createListRest(SimpleListValueDescriptor listValueDescriptor) {
		final List<Statement> stmts = new ArrayList<>(listValueDescriptor.getValues().size());
		NamedResource previous = listValueDescriptor.getValues().get(0);
		final URI nextNodeProp = SesameUtils.toSesameUri(listValueDescriptor.getNextNode()
				.getIdentifier(), vf);
		final URI context = SesameUtils.toSesameUri(listValueDescriptor.getContext(), vf);
		for (NamedResource elem : listValueDescriptor.getValues()) {
			if (elem == previous) {
				continue;
			}
			final Resource subject = SesameUtils.toSesameUri(previous.getIdentifier(), vf);
			final Resource object = SesameUtils.toSesameUri(elem.getIdentifier(), vf);
			stmts.add(vf.createStatement(subject, nextNodeProp, object, context));
			previous = elem;
		}
		return stmts;
	}
}
