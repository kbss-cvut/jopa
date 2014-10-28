package cz.cvut.kbss.jopa.oom;

import java.net.URI;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.Map;

import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.PluralAttribute;
import cz.cvut.kbss.ontodriver.exceptions.NotYetImplementedException;
import cz.cvut.kbss.ontodriver_new.model.Assertion;
import cz.cvut.kbss.ontodriver_new.model.Axiom;
import cz.cvut.kbss.ontodriver_new.model.Value;

class PluralObjectPropertyStrategy<X> extends FieldStrategy<Attribute<? super X, ?>, X> {

	private final PluralAttribute<? super X, ?, ?> pluralAtt;
	private Collection<Object> values;

	public PluralObjectPropertyStrategy(EntityType<X> et, Attribute<? super X, ?> att,
			Descriptor descriptor, EntityMappingHelper mapper) {
		super(et, att, descriptor, mapper);
		this.pluralAtt = (PluralAttribute<? super X, ?, ?>) attribute;
		initCollection();
	}

	private void initCollection() {
		switch (pluralAtt.getCollectionType()) {
		case COLLECTION:
		case LIST:
			this.values = new ArrayList<>();
			break;
		case SET:
			this.values = new HashSet<>();
			break;
		default:
			throw new NotYetImplementedException("This type of collection is not supported yet.");
		}
	}

	@Override
	void addValueFromAxiom(Axiom<?> ax) {
		final URI valueIdentifier = (URI) ax.getValue().getValue();
		final Object value = mapper.getEntityFromCacheOrOntology(pluralAtt.getBindableJavaType(),
				valueIdentifier, descriptor);
		values.add(value);

	}

	@Override
	void buildInstanceFieldValue(Object instance) throws IllegalArgumentException,
			IllegalAccessException {
		setValueOnInstance(instance, values);
	}

	@Override
	Map<Assertion, Collection<Value<?>>> extractAttributeValuesFromInstance(X instance)
			throws IllegalArgumentException, IllegalAccessException {
		final Object value = extractFieldValueFromInstance(instance);
		if (value == null) {
			return Collections.<Assertion, Collection<Value<?>>> singletonMap(createAssertion(),
					Collections.<Value<?>> singleton(Value.nullValue()));
		}
		assert value instanceof Collection;
		final Collection<?> valueCollection = (Collection<?>) value;

		final PluralAttributeValueExtractor extractor = PluralAttributeValueExtractor.create(this,
				pluralAtt);
		return extractor.extractValues(valueCollection, descriptor);
	}

	@Override
	Assertion createAssertion() {
		return Assertion.createObjectPropertyAssertion(pluralAtt.getIRI().toURI(),
				attribute.isInferred());
	}
}
