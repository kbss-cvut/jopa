package cz.cvut.kbss.ontodriver.sesame;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;

import org.openrdf.model.Resource;
import org.openrdf.model.Statement;
import org.openrdf.model.URI;
import org.openrdf.model.ValueFactory;

import cz.cvut.kbss.ontodriver.exceptions.IntegrityConstraintViolatedException;
import cz.cvut.kbss.ontodriver.sesame.connector.Connector;
import cz.cvut.kbss.ontodriver.sesame.exceptions.SesameDriverException;
import cz.cvut.kbss.ontodriver_new.descriptors.ReferencedListDescriptor;
import cz.cvut.kbss.ontodriver_new.descriptors.ReferencedListValueDescriptor;
import cz.cvut.kbss.ontodriver_new.model.Assertion;
import cz.cvut.kbss.ontodriver_new.model.Axiom;
import cz.cvut.kbss.ontodriver_new.model.AxiomImpl;
import cz.cvut.kbss.ontodriver_new.model.NamedResource;
import cz.cvut.kbss.ontodriver_new.model.Value;

public class ReferencedListHandler extends
		ListHandler<ReferencedListDescriptor, ReferencedListValueDescriptor> {

	private ReferencedListDescriptor listDescriptor;
	private int sequenceCounter = 0;

	ReferencedListHandler(Connector connector, ValueFactory vf) {
		super(connector, vf);
	}

	@Override
	Collection<Axiom<?>> loadList(ReferencedListDescriptor listDescriptor)
			throws SesameDriverException {
		this.listDescriptor = listDescriptor;
		final List<Axiom<?>> axioms = new ArrayList<>();
		Resource headNode = loadListHead(axioms);
		if (headNode == null) {
			return Collections.emptyList();
		}
		axioms.addAll(loadListRest(headNode));
		return axioms;
	}

	private Resource loadListHead(List<Axiom<?>> axioms) throws SesameDriverException {
		final URI owner = owner(listDescriptor);
		final URI hasList = hasList(listDescriptor);
		final URI context = context(listDescriptor);
		final Collection<Statement> heads = connector.findStatements(owner, hasList, null,
				listDescriptor.getListProperty().isInferred(), context);
		if (heads.isEmpty()) {
			return null;
		}
		final Resource headNode = extractListNode(heads, listDescriptor.getListProperty());
		final URI hasContent = hasContent(listDescriptor);
		final Resource head = getNodeContent(headNode, hasContent, context);
		axioms.add(createAxiom(owner, listDescriptor.getNodeContent(), head));
		return headNode;
	}

	private URI hasContent(ReferencedListDescriptor listDescriptor) {
		return sesameUri(listDescriptor.getNodeContent().getIdentifier());
	}

	private Resource getNodeContent(final Resource node, final URI hasContent, final URI context)
			throws SesameDriverException {
		final Collection<Statement> nodeContents = connector.findStatements(node, hasContent, null,
				listDescriptor.getNodeContent().isInferred(), context);
		if (nodeContents.isEmpty()) {
			throw new IntegrityConstraintViolatedException("Referenced list node " + node
					+ " is missing content.");
		}
		final Resource content = extractListNode(nodeContents, listDescriptor.getNodeContent());
		return content;
	}

	private Axiom<java.net.URI> createAxiom(Resource subject, Assertion assertion, Resource value) {
		return new AxiomImpl<java.net.URI>(NamedResource.create(SesameUtils.toJavaUri(subject)),
				assertion, new Value<java.net.URI>(SesameUtils.toJavaUri(value)));
	}

	private Collection<Axiom<java.net.URI>> loadListRest(Resource previousNode)
			throws SesameDriverException {
		final List<Axiom<java.net.URI>> axioms = new ArrayList<>();
		final URI context = context(listDescriptor);
		boolean includeInferred = listDescriptor.getNextNode().isInferred();
		final URI nextNode = hasNext(listDescriptor);
		final URI hasContent = hasContent(listDescriptor);
		boolean hasNext = true;
		do {
			final Collection<Statement> nextNodes = connector.findStatements(previousNode,
					nextNode, null, includeInferred, context);
			if (!nextNodes.isEmpty()) {
				Resource node = extractListNode(nextNodes, listDescriptor.getNextNode());
				final Resource content = getNodeContent(node, hasContent, context);
				axioms.add(createAxiom(node, listDescriptor.getNodeContent(), content));
				previousNode = node;
			} else {
				hasNext = false;
			}
		} while (hasNext);
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
}
