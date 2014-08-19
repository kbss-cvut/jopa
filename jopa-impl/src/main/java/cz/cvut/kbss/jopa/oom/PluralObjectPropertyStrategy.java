package cz.cvut.kbss.jopa.oom;

import java.net.URI;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;

import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.PluralAttribute;
import cz.cvut.kbss.ontodriver.exceptions.NotYetImplementedException;
import cz.cvut.kbss.ontodriver_new.model.Axiom;

class PluralObjectPropertyStrategy extends FieldStrategy {

	private final PluralAttribute<?, ?, ?> pluralAtt;
	private Collection<Object> values;

	public PluralObjectPropertyStrategy(EntityType<?> et, Attribute<?, ?> att,
			Descriptor descriptor, ObjectOntologyMapperImpl mapper) {
		super(et, att, descriptor, mapper);
		this.pluralAtt = (PluralAttribute<?, ?, ?>) attribute;
		initCollection();
	}

	private void initCollection() {
		final PluralAttribute<?, ?, ?> plAtt = (PluralAttribute<?, ?, ?>) attribute;
		switch (plAtt.getCollectionType()) {
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
	void addValueFromAxiom(Axiom ax) {
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
}
