package cz.cvut.kbss.jopa.oom;

import cz.cvut.kbss.jopa.exceptions.StorageAccessException;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;
import cz.cvut.kbss.ontodriver_new.Connection;
import cz.cvut.kbss.ontodriver_new.descriptors.AxiomValueDescriptor;
import cz.cvut.kbss.ontodriver_new.descriptors.ReferencedListValueDescriptor;
import cz.cvut.kbss.ontodriver_new.descriptors.SimpleListValueDescriptor;
import cz.cvut.kbss.ontodriver_new.model.Assertion;
import cz.cvut.kbss.ontodriver_new.model.NamedResource;
import cz.cvut.kbss.ontodriver_new.model.Value;

import java.net.URI;
import java.util.*;

/**
 * Gathers axiom values in descriptors for persist or update.
 * 
 * @author ledvima1
 * 
 */
class AxiomValueGatherer {

	private final AxiomValueDescriptor axiomDescriptor;
	private final Collection<SimpleListValueDescriptor> simpleListDescriptors = new ArrayList<>();
	private final Collection<ReferencedListValueDescriptor> referencedListDescriptors = new ArrayList<>();
	private Set<URI> typesToAdd;
	private Set<URI> typesToRemove;
	private URI typesContext;

	AxiomValueGatherer(NamedResource subject, URI subjectContext) {
		this.axiomDescriptor = new AxiomValueDescriptor(subject);
		axiomDescriptor.setSubjectContext(subjectContext);
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
		if (typesToAdd == null) {
			this.typesToAdd = new HashSet<>();
		}
		this.typesToAdd.addAll(types);
		this.typesContext = context;
	}

	void removeTypes(Set<URI> types, URI context) {
		if (typesToRemove == null) {
			this.typesToRemove = new HashSet<>();
		}
		this.typesToRemove.addAll(types);
		this.typesContext = context;
	}

	void persist(Connection connection) {
		try {
			connection.persist(axiomDescriptor);
			if (typesToAdd != null) {
				connection.types().addTypes(axiomDescriptor.getSubject(), typesContext, typesToAdd);
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
			if (typesToAdd != null) {
				connection.types().addTypes(axiomDescriptor.getSubject(), typesContext, typesToAdd);
			}
			if (typesToRemove != null) {
				connection.types().removeTypes(axiomDescriptor.getSubject(), typesContext, typesToRemove);
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
