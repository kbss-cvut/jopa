package cz.cvut.kbss.jopa.oom;

import java.net.URI;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Set;

import cz.cvut.kbss.jopa.exceptions.StorageAccessException;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;
import cz.cvut.kbss.ontodriver_new.Connection;
import cz.cvut.kbss.ontodriver_new.descriptors.AxiomValueDescriptor;
import cz.cvut.kbss.ontodriver_new.descriptors.ReferencedListValueDescriptor;
import cz.cvut.kbss.ontodriver_new.descriptors.SimpleListValueDescriptor;
import cz.cvut.kbss.ontodriver_new.model.Assertion;
import cz.cvut.kbss.ontodriver_new.model.Axiom;
import cz.cvut.kbss.ontodriver_new.model.NamedResource;
import cz.cvut.kbss.ontodriver_new.model.Value;

/**
 * Gathers axiom values in descriptors for persist or update.
 * 
 * @author ledvima1
 * 
 */
class AxiomValueGatherer {

	private final AxiomValueDescriptor axiomDescriptor;
	private final Collection<SimpleListValueDescriptor> simpleListDescriptors;
	private final Collection<ReferencedListValueDescriptor> referencedListDescriptors;
	private Set<URI> types;
	private URI typesContext;

	AxiomValueGatherer(NamedResource subject, URI subjectContext) {
		this.axiomDescriptor = new AxiomValueDescriptor(subject);
		axiomDescriptor.setSubjectContext(subjectContext);
		this.simpleListDescriptors = new ArrayList<>();
		this.referencedListDescriptors = new ArrayList<>();
	}

	URI getEntityContext() {
		return axiomDescriptor.getSubjectContext();
	}

	boolean containsClassAssertion() {
		return axiomDescriptor.getAssertions().contains(Assertion.createClassAssertion(false));
	}

	void addValue(Assertion assertion, Value<?> value, URI context) {
		addValues(assertion, Collections.<Value<?>> singleton(value), context);
	}

	void addValues(Assertion assertion, Collection<Value<?>> values, URI context) {
		axiomDescriptor.addAssertion(assertion);
		for (Value<?> v : values) {
			axiomDescriptor.addAssertionValue(assertion, v);
		}
		if (context != null) {
			axiomDescriptor.setAssertionContext(assertion, context);
		}
	}

	void addSimpleListValues(SimpleListValueDescriptor listDescriptor) {
		simpleListDescriptors.add(listDescriptor);
	}

	void addReferencedListValues(ReferencedListValueDescriptor listDescriptor) {
		referencedListDescriptors.add(listDescriptor);
	}

	void addTypes(Set<URI> types, URI context) {
		this.types = types;
		this.typesContext = context;
	}

	void persist(Connection connection) {
		try {
			connection.persist(axiomDescriptor);
			if (types != null) {
				connection.types().persistTypes(axiomDescriptor.getSubject(), typesContext, types);
			}
			for (SimpleListValueDescriptor d : simpleListDescriptors) {
				connection.lists().persistSimpleList(d);
			}
			for (ReferencedListValueDescriptor d : referencedListDescriptors) {
				connection.lists().persistReferencedList(d);
			}
		} catch (OntoDriverException e) {
			throw new StorageAccessException(e);
		}
	}

	void update(Connection connection) {
		try {
			connection.update(axiomDescriptor);
			if (types != null) {
				connection.types().updateTypes(axiomDescriptor.getSubject(), typesContext, types);
			}
			for (SimpleListValueDescriptor d : simpleListDescriptors) {
				connection.lists().updateSimpleList(d);
			}
			for (ReferencedListValueDescriptor d : referencedListDescriptors) {
				connection.lists().updateReferencedList(d);
			}
		} catch (OntoDriverException e) {
			throw new StorageAccessException(e);
		}
	}
}
