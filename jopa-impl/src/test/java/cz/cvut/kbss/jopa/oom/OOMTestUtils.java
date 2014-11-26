package cz.cvut.kbss.jopa.oom;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.List;
import java.util.Set;

import cz.cvut.kbss.ontodriver_new.descriptors.AxiomValueDescriptor;
import cz.cvut.kbss.ontodriver_new.descriptors.ReferencedListValueDescriptor;
import cz.cvut.kbss.ontodriver_new.descriptors.SimpleListValueDescriptor;

final class OOMTestUtils {

	private OOMTestUtils() {
		throw new AssertionError();
	}

	public static AxiomValueDescriptor getAxiomValueDescriptor(
			AxiomValueGatherer builder) throws Exception {
		final Field descriptorField = AxiomValueGatherer.class
				.getDeclaredField("axiomDescriptor");
		descriptorField.setAccessible(true);
		return (AxiomValueDescriptor) descriptorField.get(builder);
	}

	@SuppressWarnings("unchecked")
	public static List<SimpleListValueDescriptor> getSimpleListValueDescriptors(
			AxiomValueGatherer builder) throws Exception {
		final Field descriptorField = AxiomValueGatherer.class
				.getDeclaredField("simpleListDescriptors");
		descriptorField.setAccessible(true);
		return (List<SimpleListValueDescriptor>) descriptorField.get(builder);
	}

	@SuppressWarnings("unchecked")
	public static List<ReferencedListValueDescriptor> getReferencedListValueDescriptors(
			AxiomValueGatherer builder) throws Exception {
		final Field descriptorsField = AxiomValueGatherer.class
				.getDeclaredField("referencedListDescriptors");
		descriptorsField.setAccessible(true);
		return (List<ReferencedListValueDescriptor>) descriptorsField
				.get(builder);
	}

	public static Set<URI> getTypes(AxiomValueGatherer builder) throws Exception {
		final Field typesField = AxiomValueGatherer.class
				.getDeclaredField("types");
		typesField.setAccessible(true);
		return (Set<URI>) typesField.get(builder);
	}
}
