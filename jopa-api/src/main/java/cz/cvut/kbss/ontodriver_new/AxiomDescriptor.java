package cz.cvut.kbss.ontodriver_new;

import java.net.URI;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

import cz.cvut.kbss.jopa.utils.ErrorUtils;
import cz.cvut.kbss.ontodriver_new.model.NamedResource;
import cz.cvut.kbss.ontodriver_new.model.Property;

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
	private final Set<Property> properties;

	private URI subjectContext;
	private final Map<Property, URI> propertyContexts;

	public AxiomDescriptor(NamedResource subject) {
		this.subject = Objects.requireNonNull(subject, ErrorUtils.constructNPXMessage("subject"));
		this.properties = new HashSet<>();
		this.propertyContexts = new HashMap<>();
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
	 * Adds the specified property to this descriptor.
	 * 
	 * @param property
	 *            The property to add
	 * @throws NullPointerException
	 */
	public void addProperty(Property property) {
		Objects.requireNonNull(property, ErrorUtils.constructNPXMessage("property"));
		properties.add(property);
	}

	/**
	 * Sets context for the specified property. </p>
	 * 
	 * Note that the property has to be already present in this descriptor.
	 * 
	 * @param property
	 *            The property to set context for
	 * @param context
	 *            Context URI
	 * @throws IllegalArgumentException
	 *             If there is no such property in this descriptor
	 * @throws NullPointerException
	 */
	public void setPropertyContext(Property property, URI context) {
		Objects.requireNonNull(property, ErrorUtils.constructNPXMessage("property"));
		if (!properties.contains(property)) {
			throw new IllegalArgumentException("Property " + property
					+ " is not present in this loading descriptor.");
		}
		propertyContexts.put(property, context);
	}

	public NamedResource getSubject() {
		return subject;
	}

	public URI getSubjectContext() {
		return subjectContext;
	}

	/**
	 * Gets context of the specified property. </p>
	 * 
	 * If the context was not explicitly set, the same context as the subject's
	 * is returned.
	 * 
	 * @return Property context
	 */
	public URI getPropertyContext(Property property) {
		Objects.requireNonNull(property, ErrorUtils.constructNPXMessage("property"));
		if (!properties.contains(property)) {
			return subjectContext;
		}
		return propertyContexts.get(property);
	}

	/**
	 * Gets unmodifiable view of the properties in this descriptor.
	 */
	public Set<Property> getProperties() {
		return Collections.unmodifiableSet(properties);
	}
	
	// TODO Override hashCode and equals

	@Override
	public String toString() {
		final StringBuilder sb = new StringBuilder("Descriptor ");
		sb.append(subject);
		if (subjectContext != null) {
			sb.append(" - ").append(subjectContext);
		}
		if (!propertyContexts.isEmpty()) {
			sb.append(", properties: ").append(propertyContexts);
		}
		return sb.toString();
	}
}
