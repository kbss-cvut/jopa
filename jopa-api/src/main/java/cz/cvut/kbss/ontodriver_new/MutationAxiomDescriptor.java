package cz.cvut.kbss.ontodriver_new;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import cz.cvut.kbss.jopa.utils.ErrorUtils;
import cz.cvut.kbss.ontodriver_new.model.NamedResource;
import cz.cvut.kbss.ontodriver_new.model.Property;
import cz.cvut.kbss.ontodriver_new.model.Value;

// TODO Rename this class
public class MutationAxiomDescriptor extends AxiomDescriptor {

	private final Map<Property, List<Value<?>>> values;

	public MutationAxiomDescriptor(NamedResource subject) {
		super(subject);
		this.values = new HashMap<>();
	}

	/**
	 * Adds a new value for the specified property. </p>
	 * 
	 * @param property
	 *            The property
	 * @param value
	 *            The value to add
	 * @return This descriptor
	 * @throws NullPointerException
	 *             if either of the arguments is {@code null}
	 */
	public MutationAxiomDescriptor addPropertyValue(Property property, Value<?> value) {
		Objects.requireNonNull(property, ErrorUtils.constructNPXMessage("property"));
		Objects.requireNonNull(value, ErrorUtils.constructNPXMessage("value"));

		final List<Value<?>> propertyValues = getPropertyList(property);
		propertyValues.add(value);
		return this;
	}

	private List<Value<?>> getPropertyList(Property property) {
		assert property != null;
		if (!values.containsKey(property)) {
			values.put(property, new ArrayList<Value<?>>());
		}
		return values.get(property);
	}
	
	// TODO Add hashCode and equals
}
