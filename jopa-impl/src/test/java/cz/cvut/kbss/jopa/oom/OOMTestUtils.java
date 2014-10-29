package cz.cvut.kbss.jopa.oom;

import java.lang.reflect.Field;
import java.util.List;

import cz.cvut.kbss.ontodriver_new.descriptors.AxiomValueDescriptor;
import cz.cvut.kbss.ontodriver_new.descriptors.SimpleListValueDescriptor;

final class OOMTestUtils {

	private OOMTestUtils() {
		throw new AssertionError();
	}

	public static AxiomValueDescriptor getAxiomValueDescriptor(AxiomValueGatherer builder)
			throws Exception {
		final Field descriptorField = AxiomValueGatherer.class.getDeclaredField("axiomDescriptor");
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
}
