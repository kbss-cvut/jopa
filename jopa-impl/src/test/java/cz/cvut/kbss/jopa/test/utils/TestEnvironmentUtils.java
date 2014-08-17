package cz.cvut.kbss.jopa.test.utils;

import static org.mockito.Mockito.when;

import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.net.URI;
import java.util.Collections;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Random;
import java.util.Set;

import cz.cvut.kbss.jopa.model.IRI;
import cz.cvut.kbss.jopa.model.annotations.OWLDataProperty;
import cz.cvut.kbss.jopa.model.annotations.OWLObjectProperty;
import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.model.metamodel.Attribute.PersistentAttributeType;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.PropertiesSpecification;
import cz.cvut.kbss.jopa.model.metamodel.TypesSpecification;
import cz.cvut.kbss.jopa.sessions.ObjectChangeSet;
import cz.cvut.kbss.jopa.sessions.ObjectChangeSetImpl;
import cz.cvut.kbss.jopa.test.OWLClassA;
import cz.cvut.kbss.jopa.test.OWLClassB;
import cz.cvut.kbss.jopa.test.OWLClassD;

public final class TestEnvironmentUtils {

	private static Random random;

	private TestEnvironmentUtils() {
		throw new AssertionError();
	}

	static {
		random = new Random();
	}

	public static ObjectChangeSet createObjectChangeSet(Object original,
			Object clone, URI context) {
		return new ObjectChangeSetImpl(original, clone, context);
	}

	public static int randomInt(int max) {
		return random.nextInt(max);
	}

	public static boolean arePropertiesEqual(Map<String, Set<String>> pOne,
			Map<String, Set<String>> pTwo) {
		if (pOne.size() != pTwo.size()) {
			return false;
		}
		for (Entry<String, Set<String>> e : pOne.entrySet()) {
			if (!pTwo.containsKey(e.getKey())) {
				return false;
			}
			final Set<String> set = pTwo.get(e.getKey());
			if (!e.getValue().equals(set)) {
				return false;
			}
		}
		return true;
	}

	/**
	 * Sets value of field {@code field} on the {@code target} object to the
	 * specified {@code mock}. </p>
	 * 
	 * This method also works for final fields. Note that in case static fields
	 * are set, it is the responsibility of the client to reset the field to the
	 * original value in test cleanup.
	 * 
	 * @param target
	 *            Target instance, can be {@code null} for static fields
	 * @param field
	 *            Field to set
	 * @param mock
	 *            The new value
	 */
	public static void setMock(Object target, Field field, Object mock)
			throws Exception {
		assert field != null;
		field.setAccessible(true);
		removeFinalModifier(field);
		field.set(target, mock);
	}

	private static void removeFinalModifier(Field field) throws Exception {
		// remove final modifier from field
		Field modifiersField = Field.class.getDeclaredField("modifiers");
		modifiersField.setAccessible(true);
		modifiersField.setInt(field, field.getModifiers() & ~Modifier.FINAL);
	}

	/**
	 * Initializes the specified mock objects to return reasonable values.
	 */
	public static void initOWLClassAMocks(EntityType<OWLClassA> etMock,
			Attribute strAttMock, TypesSpecification typesMock)
			throws NoSuchFieldException, SecurityException {
		when(etMock.getAttribute(OWLClassA.getStrAttField().getName()))
				.thenReturn(strAttMock);
		when(etMock.getTypes()).thenReturn(typesMock);
		when(etMock.getAttributes())
				.thenReturn(
						Collections
								.<Attribute<? super OWLClassA, ?>> singleton(strAttMock));
		when(strAttMock.getJavaField()).thenReturn(OWLClassA.getStrAttField());
		final String stringAttIri = OWLClassA.getStrAttField()
				.getAnnotation(OWLDataProperty.class).iri();
		when(strAttMock.getIRI()).thenReturn(IRI.create(stringAttIri));
		when(strAttMock.getPersistentAttributeType()).thenReturn(
				PersistentAttributeType.DATA);
		when(typesMock.getJavaField()).thenReturn(OWLClassA.getTypesField());
	}

	/**
	 * Initializes the specified mock objects to return reasonable values.
	 */
	public static void initOWLClassBMocks(EntityType<OWLClassB> etMock,
			Attribute strAttMock, PropertiesSpecification propsMock)
			throws NoSuchFieldException, SecurityException {
		when(etMock.getAttribute(OWLClassB.getStrAttField().getName()))
				.thenReturn(strAttMock);
		when(etMock.getProperties()).thenReturn(propsMock);
		when(etMock.getAttributes())
				.thenReturn(
						Collections
								.<Attribute<? super OWLClassB, ?>> singleton(strAttMock));
		when(strAttMock.getJavaField()).thenReturn(OWLClassB.getStrAttField());
		final String stringAttIri = OWLClassB.getStrAttField()
				.getAnnotation(OWLDataProperty.class).iri();
		when(strAttMock.getIRI()).thenReturn(IRI.create(stringAttIri));
		when(strAttMock.getPersistentAttributeType()).thenReturn(
				PersistentAttributeType.DATA);
		when(propsMock.getJavaField()).thenReturn(
				OWLClassB.getPropertiesField());
	}

	/**
	 * Initializes the specified mock objects to return reasonable values.
	 */
	public static void initOWLClassDMocks(EntityType<OWLClassD> etMock,
			Attribute clsAMock) throws NoSuchFieldException, SecurityException {
		when(etMock.getAttribute(OWLClassD.getOwlClassAField().getName()))
				.thenReturn(clsAMock);
		when(etMock.getAttributes()).thenReturn(
				Collections
						.<Attribute<? super OWLClassD, ?>> singleton(clsAMock));
		when(clsAMock.getJavaField()).thenReturn(OWLClassD.getOwlClassAField());
		when(clsAMock.getPersistentAttributeType()).thenReturn(
				PersistentAttributeType.OBJECT);
		final String clsAIri = OWLClassD.getOwlClassAField()
				.getAnnotation(OWLObjectProperty.class).iri();
		when(clsAMock.getIRI()).thenReturn(IRI.create(clsAIri));
	}
}
