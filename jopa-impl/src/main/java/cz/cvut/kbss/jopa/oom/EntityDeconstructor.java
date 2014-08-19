package cz.cvut.kbss.jopa.oom;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.Set;

import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.ontodriver_new.MutationAxiomDescriptor;
import cz.cvut.kbss.ontodriver_new.model.Assertion;
import cz.cvut.kbss.ontodriver_new.model.NamedResource;
import cz.cvut.kbss.ontodriver_new.model.Value;

class EntityDeconstructor {

	<T> MutationAxiomDescriptor mapEntityToAxioms(URI primaryKey, T entity, EntityType<T> et,
			Descriptor descriptor) {
		assert primaryKey != null;
		final MutationAxiomDescriptor axiomDescriptor = new MutationAxiomDescriptor(
				NamedResource.create(primaryKey));
		try {
			addClassAssertions(axiomDescriptor, entity, et);
			// TODO
		} catch (IllegalArgumentException | IllegalAccessException e) {
			throw new EntityDeconstructionException(e);
		}
		return axiomDescriptor;
	}

	private <T> void addClassAssertions(MutationAxiomDescriptor axiomDescriptor, T entity,
			EntityType<T> et) throws IllegalArgumentException, IllegalAccessException {
		final OWLClass clsType = entity.getClass().getAnnotation(OWLClass.class);
		assert clsType != null;
		axiomDescriptor.addAssertionValue(Assertion.createClassAssertion(false),
				new Value<URI>(URI.create(clsType.iri())));
		if (et.getTypes() != null) {
			final Field typesField = et.getTypes().getJavaField();
			typesField.setAccessible(true);
			final Set<String> types = (Set<String>) typesField.get(entity);
			if (types == null || types.isEmpty()) {
				return;
			}
			for (String t : types) {
				try {
					final URI typeUri = URI.create(t);
					axiomDescriptor.addAssertionValue(
							Assertion.createClassAssertion(et.getTypes().isInferred()),
							new Value<URI>(typeUri));
				} catch (IllegalArgumentException e) {
					throw new EntityDeconstructionException("The type " + t
							+ " is not a valid URI.", e);
				}
			}
		}
	}
}
