package cz.cvut.kbss.ontodriver.sesame;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.openrdf.model.Literal;
import org.openrdf.model.Resource;
import org.openrdf.model.Statement;
import org.openrdf.model.URI;
import org.openrdf.model.ValueFactory;

import cz.cvut.kbss.ontodriver.sesame.connector.Connector;
import cz.cvut.kbss.ontodriver.sesame.exceptions.SesameDriverException;
import cz.cvut.kbss.ontodriver_new.descriptors.AxiomDescriptor;
import cz.cvut.kbss.ontodriver_new.model.Assertion;
import cz.cvut.kbss.ontodriver_new.model.Axiom;
import cz.cvut.kbss.ontodriver_new.model.AxiomImpl;
import cz.cvut.kbss.ontodriver_new.model.NamedResource;
import cz.cvut.kbss.ontodriver_new.model.Value;

class AxiomLoader {

	private final Connector connector;
	private final ValueFactory valueFactory;

	private Map<URI, Assertion> propertyToAssertion;
	private Assertion unspecifiedProperty;

	AxiomLoader(Connector connector, ValueFactory valueFactory) {
		this.connector = connector;
		this.valueFactory = valueFactory;
		this.propertyToAssertion = new HashMap<>();
	}

	Collection<Axiom<?>> loadAxioms(AxiomDescriptor axiomDescriptor) throws SesameDriverException {
		Collection<Statement> statements = findStatements(axiomDescriptor);
		final List<Axiom<?>> axioms = transformStatementsToAxioms(statements);
		return axioms;
	}

	private Collection<Statement> findStatements(AxiomDescriptor descriptor)
			throws SesameDriverException {
		final Collection<Statement> result = new ArrayList<>();
		final Resource subject = SesameUtils.toSesameUri(descriptor.getSubject().getIdentifier(),
				valueFactory);
		for (Assertion assertion : descriptor.getAssertions()) {
			final URI property = getPropertyUri(assertion);

			final URI context = SesameUtils.toSesameUri(descriptor.getAssertionContext(assertion),
					valueFactory);
			result.addAll(connector.findStatements(subject, property, null, assertion.isInferred(),
					context));
		}
		return result;
	}

	private URI getPropertyUri(Assertion assertion) {
		if (assertion.equals(Assertion.createUnspecifiedPropertyAssertion(assertion.isInferred()))) {
			this.unspecifiedProperty = assertion;
			return null;
		}
		final URI property = SesameUtils.toSesameUri(assertion.getIdentifier(), valueFactory);
		propertyToAssertion.put(property, assertion);
		return property;
	}

	private List<Axiom<?>> transformStatementsToAxioms(Collection<Statement> statements) {
		final List<Axiom<?>> axioms = new ArrayList<>(statements.size());
		final Map<Resource, NamedResource> subjects = new HashMap<>();
		for (Statement stmt : statements) {
			final Axiom<?> axiom = createAxiom(stmt, subjects);
			if (axiom == null) {
				continue;
			}
			axioms.add(axiom);
		}
		return axioms;
	}

	private Axiom<?> createAxiom(Statement stmt, Map<Resource, NamedResource> knownSubjects) {
		if (!knownSubjects.containsKey(stmt.getSubject())) {
			knownSubjects.put(stmt.getSubject(),
					NamedResource.create(SesameUtils.toJavaUri(stmt.getSubject())));
		}
		final NamedResource subject = knownSubjects.get(stmt.getSubject());
		Assertion assertion = propertyToAssertion.get(stmt.getPredicate());
		if (assertion == null) {
			if (unspecifiedProperty == null) {
				return null;
			} else {
				assertion = unspecifiedProperty;
			}
		}
		Value<?> val = null;
		switch (assertion.getType()) {
		case ANNOTATION_PROPERTY:
		case DATA_PROPERTY:
			if (!(stmt.getObject() instanceof Literal)) {
				return null;
			}
			val = new Value<>(SesameUtils.getDataPropertyValue((Literal) stmt.getObject()));
			break;
		case CLASS:
			if (!(stmt.getObject() instanceof Resource)) {
				return null;
			}
			val = new Value<java.net.URI>(SesameUtils.toJavaUri((Resource) stmt.getObject()));
		case OBJECT_PROPERTY:
			if (!(stmt.getObject() instanceof Resource)) {
				return null;
			}
			val = new Value<NamedResource>(NamedResource.create(stmt.getObject().stringValue()));
			break;
		case PROPERTY:
			assertion = Assertion.createPropertyAssertion(
					SesameUtils.toJavaUri(stmt.getPredicate()), assertion.isInferred());
			val = resovelValue(stmt.getObject());
			break;

		}
		return new AxiomImpl<>(subject, assertion, val);
	}

	private Value<?> resovelValue(org.openrdf.model.Value object) {
		if (object instanceof Literal) {
			return new Value<>(SesameUtils.getDataPropertyValue((Literal) object));
		} else {
			return new Value<java.net.URI>(SesameUtils.toJavaUri((Resource) object));
		}
	}
}
