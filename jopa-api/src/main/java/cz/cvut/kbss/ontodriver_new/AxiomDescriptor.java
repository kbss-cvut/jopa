package cz.cvut.kbss.ontodriver_new;

import java.net.URI;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

import cz.cvut.kbss.jopa.utils.ErrorUtils;
import cz.cvut.kbss.ontodriver_new.model.Assertion;
import cz.cvut.kbss.ontodriver_new.model.NamedResource;

/**
 * This descriptor specifies subject and properties of axioms to search for in
 * the ontology. </p>
 * 
 * Additionally, it can specify context URIs for both the descriptor and
 * individual properties so that the underlying driver knows where to look for
 * the corresponding axioms.
 * 
 * @author ledvima1
 * 
 */
public class AxiomDescriptor {

	private final NamedResource subject;
	private final Set<Assertion> assertions;

	private URI subjectContext;
	private final Map<Assertion, URI> assertionContexts;

	public AxiomDescriptor(NamedResource subject) {
		this.subject = Objects.requireNonNull(subject, ErrorUtils.constructNPXMessage("subject"));
		this.assertions = new HashSet<>();
		this.assertionContexts = new HashMap<>();
	}

	/**
	 * Sets subject context.
	 * 
	 * @param context
	 *            The context to use for subject
	 */
	public void setSubjectContext(URI context) {
		this.subjectContext = context;
	}

	/**
	 * Adds the specified assertion to this descriptor.
	 * 
	 * @param assertion
	 *            The assertion to add
	 * @throws NullPointerException
	 */
	public void addAssertion(Assertion assertion) {
		Objects.requireNonNull(assertion, ErrorUtils.constructNPXMessage("assertion"));
		assertions.add(assertion);
	}

	/**
	 * Sets context for the specified assertion. </p>
	 * 
	 * Note that the assertion has to be already present in this descriptor.
	 * 
	 * @param assertion
	 *            The property to set context for
	 * @param context
	 *            Context URI
	 * @throws IllegalArgumentException
	 *             If there is no such assertion in this descriptor
	 * @throws NullPointerException
	 */
	public void setAssertionContext(Assertion assertion, URI context) {
		Objects.requireNonNull(assertion, ErrorUtils.constructNPXMessage("assertion"));
		if (!assertions.contains(assertion)) {
			throw new IllegalArgumentException("Assertion " + assertion
					+ " is not present in this loading descriptor.");
		}
		assertionContexts.put(assertion, context);
	}

	public NamedResource getSubject() {
		return subject;
	}

	public URI getSubjectContext() {
		return subjectContext;
	}

	/**
	 * Gets context of the specified assertion. </p>
	 * 
	 * If the context was not explicitly set, the same context as the subject's
	 * is returned.
	 * 
	 * @return Assertion context
	 */
	public URI getAssertionContext(Assertion assertion) {
		Objects.requireNonNull(assertion, ErrorUtils.constructNPXMessage("assertion"));
		if (!assertionContexts.containsKey(assertion)) {
			return subjectContext;
		}
		return assertionContexts.get(assertion);
	}

	/**
	 * Gets unmodifiable view of the properties in this descriptor.
	 */
	public Set<Assertion> getAssertions() {
		return Collections.unmodifiableSet(assertions);
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + subject.hashCode();
		result = prime * result + ((subjectContext == null) ? 0 : subjectContext.hashCode());
		result = prime * result + assertions.hashCode();
		result = prime * result + assertionContexts.hashCode();
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		AxiomDescriptor other = (AxiomDescriptor) obj;
		if (!subject.equals(other.subject))
			return false;
		if (subjectContext == null) {
			if (other.subjectContext != null)
				return false;
		} else if (!subjectContext.equals(other.subjectContext))
			return false;
		if (!assertions.equals(other.assertions))
			return false;
		if (!assertionContexts.equals(other.assertionContexts))
			return false;
		return true;
	}

	@Override
	public String toString() {
		final StringBuilder sb = new StringBuilder();
		sb.append(subject);
		if (subjectContext != null) {
			sb.append(" - ").append(subjectContext);
		}
		if (!assertionContexts.isEmpty()) {
			sb.append(", properties: ").append(assertionContexts);
		} else {
			sb.append(", properties: ").append(assertions);
		}
		return sb.toString();
	}
}
