package cz.cvut.kbss.jopa.test.utils;

import static org.mockito.Mockito.when;

import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.net.URI;
import java.util.*;
import java.util.Map.Entry;

import cz.cvut.kbss.jopa.model.IRI;
import cz.cvut.kbss.jopa.model.annotations.*;
import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.model.metamodel.Attribute.PersistentAttributeType;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.Identifier;
import cz.cvut.kbss.jopa.model.metamodel.ListAttribute;
import cz.cvut.kbss.jopa.model.metamodel.PluralAttribute;
import cz.cvut.kbss.jopa.model.metamodel.PluralAttribute.CollectionType;
import cz.cvut.kbss.jopa.model.metamodel.PropertiesSpecification;
import cz.cvut.kbss.jopa.model.metamodel.TypesSpecification;
import cz.cvut.kbss.jopa.oom.model.OWLClassL;
import cz.cvut.kbss.jopa.sessions.ObjectChangeSet;
import cz.cvut.kbss.jopa.sessions.ObjectChangeSetImpl;
import cz.cvut.kbss.jopa.test.OWLClassA;
import cz.cvut.kbss.jopa.test.OWLClassB;
import cz.cvut.kbss.jopa.test.OWLClassC;
import cz.cvut.kbss.jopa.test.OWLClassD;
import cz.cvut.kbss.jopa.test.OWLClassE;
import cz.cvut.kbss.jopa.test.OWLClassJ;
import cz.cvut.kbss.jopa.test.OWLClassK;

public final class TestEnvironmentUtils {

    private static Random random;

    private TestEnvironmentUtils() {
        throw new AssertionError();
    }

    static {
        random = new Random();
    }

    public static ObjectChangeSet createObjectChangeSet(Object original, Object clone, URI context) {
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
     * <p/>
     * This method also works for final fields. Note that in case static fields
     * are set, it is the responsibility of the client to reset the field to the
     * original value in test cleanup.
     *
     * @param target Target instance, can be {@code null} for static fields
     * @param field  Field to set
     * @param mock   The new value
     */
    public static void setMock(Object target, Field field, Object mock) throws Exception {
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
    public static void initOWLClassAMocks(EntityType<OWLClassA> etMock, Attribute strAttMock,
                                          TypesSpecification typesMock) throws NoSuchFieldException, SecurityException {
        when(etMock.getJavaType()).thenReturn(OWLClassA.class);
        when(etMock.getIRI()).thenReturn(IRI.create(OWLClassA.getClassIri()));
        when(etMock.getAttribute(OWLClassA.getStrAttField().getName())).thenReturn(strAttMock);
        when(etMock.getTypes()).thenReturn(typesMock);
        when(etMock.getAttributes()).thenReturn(
                Collections.<Attribute<? super OWLClassA, ?>>singleton(strAttMock));
        when(strAttMock.getJavaField()).thenReturn(OWLClassA.getStrAttField());
        final String stringAttIri = OWLClassA.getStrAttField().getAnnotation(OWLDataProperty.class)
                .iri();
        when(strAttMock.getIRI()).thenReturn(IRI.create(stringAttIri));
        when(strAttMock.getPersistentAttributeType()).thenReturn(PersistentAttributeType.DATA);
        when(typesMock.getJavaField()).thenReturn(OWLClassA.getTypesField());
    }

    /**
     * Initializes the specified mock objects to return reasonable values.
     */
    public static void initOWLClassBMocks(EntityType<OWLClassB> etMock, Attribute strAttMock,
                                          PropertiesSpecification propsMock) throws NoSuchFieldException, SecurityException {
        when(etMock.getJavaType()).thenReturn(OWLClassB.class);
        when(etMock.getIRI()).thenReturn(IRI.create(OWLClassB.getClassIri()));
        when(etMock.getAttribute(OWLClassB.getStrAttField().getName())).thenReturn(strAttMock);
        when(etMock.getProperties()).thenReturn(propsMock);
        when(etMock.getAttributes()).thenReturn(
                Collections.<Attribute<? super OWLClassB, ?>>singleton(strAttMock));
        when(strAttMock.getJavaField()).thenReturn(OWLClassB.getStrAttField());
        final String stringAttIri = OWLClassB.getStrAttField().getAnnotation(OWLDataProperty.class)
                .iri();
        when(strAttMock.getIRI()).thenReturn(IRI.create(stringAttIri));
        when(strAttMock.getPersistentAttributeType()).thenReturn(PersistentAttributeType.DATA);
        when(propsMock.getJavaField()).thenReturn(OWLClassB.getPropertiesField());
    }

    public static void initOWLClassCMocks(EntityType<OWLClassC> etMock,
                                          ListAttribute simpleListMock, ListAttribute refListMock, Identifier idMock)
            throws NoSuchFieldException, SecurityException {
        when(etMock.getJavaType()).thenReturn(OWLClassC.class);
        when(etMock.getIRI()).thenReturn(IRI.create(OWLClassC.getClassIri()));
        when(etMock.getAttribute(OWLClassC.getSimpleListField().getName())).thenReturn(
                simpleListMock);
        when(etMock.getAttribute(OWLClassC.getRefListField().getName())).thenReturn(refListMock);
        final Set<Attribute<? super OWLClassC, ?>> atts = new HashSet<>();
        atts.add(simpleListMock);
        atts.add(refListMock);
        when(etMock.getAttributes()).thenReturn(atts);
        when(simpleListMock.getJavaField()).thenReturn(OWLClassC.getSimpleListField());
        when(refListMock.getJavaField()).thenReturn(OWLClassC.getRefListField());
        String attIri = OWLClassC.getSimpleListField().getAnnotation(OWLObjectProperty.class).iri();
        when(simpleListMock.getIRI()).thenReturn(IRI.create(attIri));
        String hasListAttIri = OWLClassC.getSimpleListField().getAnnotation(Sequence.class)
                .ClassOWLListIRI();
        when(simpleListMock.getSequenceType()).thenReturn(SequenceType.simple);
        when(simpleListMock.getCollectionType()).thenReturn(CollectionType.LIST);
        when(simpleListMock.getOWLListClass()).thenReturn(IRI.create(hasListAttIri));
        String hasNextIri = OWLClassC.getSimpleListField().getAnnotation(Sequence.class)
                .ObjectPropertyHasNextIRI();
        when(simpleListMock.getOWLObjectPropertyHasNextIRI()).thenReturn(IRI.create(hasNextIri));
        when(simpleListMock.getBindableJavaType()).thenReturn(OWLClassA.class);
        when(simpleListMock.getPersistentAttributeType())
                .thenReturn(PersistentAttributeType.OBJECT);
        when(simpleListMock.isCollection()).thenReturn(Boolean.TRUE);

        hasListAttIri = OWLClassC.getRefListField().getAnnotation(Sequence.class).ClassOWLListIRI();
        when(refListMock.getSequenceType()).thenReturn(SequenceType.referenced);
        when(refListMock.getCollectionType()).thenReturn(CollectionType.LIST);
        when(refListMock.getOWLListClass()).thenReturn(IRI.create(hasListAttIri));
        hasNextIri = OWLClassC.getRefListField().getAnnotation(Sequence.class)
                .ObjectPropertyHasNextIRI();
        when(refListMock.getOWLObjectPropertyHasNextIRI()).thenReturn(IRI.create(hasNextIri));
        final String contentIri = OWLClassC.getRefListField().getAnnotation(Sequence.class)
                .ObjectPropertyHasContentsIRI();
        when(refListMock.getOWLPropertyHasContentsIRI()).thenReturn(IRI.create(contentIri));
        attIri = OWLClassC.getRefListField().getAnnotation(OWLObjectProperty.class).iri();
        when(refListMock.getIRI()).thenReturn(IRI.create(attIri));
        when(refListMock.getBindableJavaType()).thenReturn(OWLClassA.class);
        when(refListMock.getPersistentAttributeType()).thenReturn(PersistentAttributeType.OBJECT);
        when(refListMock.isCollection()).thenReturn(Boolean.TRUE);

        when(etMock.getIdentifier()).thenReturn(idMock);
        when(idMock.getJavaField()).thenReturn(OWLClassC.class.getDeclaredField("uri"));
    }

    public static void initOWLClassEMocks(EntityType<OWLClassE> etMock, Attribute strAttMock,
                                          Identifier idMock) throws NoSuchFieldException, SecurityException {
        when(etMock.getJavaType()).thenReturn(OWLClassE.class);
        when(etMock.getIRI()).thenReturn(IRI.create(OWLClassE.getClassIri()));
        when(etMock.getAttribute(OWLClassE.getStrAttField().getName())).thenReturn(strAttMock);
        when(etMock.getAttributes()).thenReturn(
                Collections.<Attribute<? super OWLClassE, ?>>singleton(strAttMock));
        when(strAttMock.getJavaField()).thenReturn(OWLClassB.getStrAttField());
        final String stringAttIri = OWLClassB.getStrAttField().getAnnotation(OWLDataProperty.class)
                .iri();
        when(strAttMock.getIRI()).thenReturn(IRI.create(stringAttIri));
        when(strAttMock.getPersistentAttributeType()).thenReturn(PersistentAttributeType.DATA);
        when(etMock.getIdentifier()).thenReturn(idMock);
        when(idMock.getJavaField()).thenReturn(OWLClassE.class.getDeclaredField("uri"));
    }

    /**
     * Initializes the specified mock objects to return reasonable values.
     */
    public static void initOWLClassDMocks(EntityType<OWLClassD> etMock, Attribute clsAMock)
            throws NoSuchFieldException, SecurityException {
        when(etMock.getJavaType()).thenReturn(OWLClassD.class);
        when(etMock.getIRI()).thenReturn(IRI.create(OWLClassD.getClassIri()));
        when(etMock.getAttribute(OWLClassD.getOwlClassAField().getName())).thenReturn(clsAMock);
        when(etMock.getAttributes()).thenReturn(
                Collections.<Attribute<? super OWLClassD, ?>>singleton(clsAMock));
        when(clsAMock.getJavaField()).thenReturn(OWLClassD.getOwlClassAField());
        when(clsAMock.getPersistentAttributeType()).thenReturn(PersistentAttributeType.OBJECT);
        final String clsAIri = OWLClassD.getOwlClassAField().getAnnotation(OWLObjectProperty.class)
                .iri();
        when(clsAMock.getIRI()).thenReturn(IRI.create(clsAIri));
        when(clsAMock.getJavaType()).thenReturn(OWLClassA.class);
    }

    public static void initOWLClassJMocks(EntityType<OWLClassJ> etMock, PluralAttribute setAMock,
                                          Identifier idMock) throws NoSuchFieldException, SecurityException {
        when(etMock.getJavaType()).thenReturn(OWLClassJ.class);
        when(etMock.getIRI()).thenReturn(IRI.create(OWLClassJ.getClassIri()));
        when(etMock.getAttribute(OWLClassJ.getOwlClassAField().getName())).thenReturn(setAMock);
        when(etMock.getAttributes()).thenReturn(
                Collections.<Attribute<? super OWLClassJ, ?>>singleton(setAMock));
        when(setAMock.getJavaField()).thenReturn(OWLClassJ.getOwlClassAField());
        when(setAMock.getPersistentAttributeType()).thenReturn(PersistentAttributeType.OBJECT);
        final String clsAIri = OWLClassJ.getOwlClassAField().getAnnotation(OWLObjectProperty.class)
                .iri();
        when(setAMock.getIRI()).thenReturn(IRI.create(clsAIri));
        when(setAMock.getJavaType()).thenReturn(Set.class);
        when(setAMock.isCollection()).thenReturn(Boolean.TRUE);
        when(setAMock.getCollectionType()).thenReturn(CollectionType.SET);
        when(setAMock.getBindableJavaType()).thenReturn(OWLClassA.class);
        when(etMock.getIdentifier()).thenReturn(idMock);
        when(idMock.getJavaField()).thenReturn(OWLClassJ.class.getDeclaredField("uri"));
    }

    public static void initOWLClassKMocks(EntityType<OWLClassK> etMock, Attribute clsEMock,
                                          Identifier idMock) throws NoSuchFieldException, SecurityException {
        when(etMock.getJavaType()).thenReturn(OWLClassK.class);
        when(etMock.getIRI()).thenReturn(IRI.create(OWLClassK.getClassIri()));
        when(etMock.getAttribute(OWLClassK.getOwlClassEField().getName())).thenReturn(clsEMock);
        when(etMock.getAttributes()).thenReturn(
                Collections.<Attribute<? super OWLClassK, ?>>singleton(clsEMock));
        when(clsEMock.getJavaField()).thenReturn(OWLClassK.getOwlClassEField());
        when(clsEMock.getPersistentAttributeType()).thenReturn(PersistentAttributeType.OBJECT);
        final String clsEIri = OWLClassK.getOwlClassEField().getAnnotation(OWLObjectProperty.class)
                .iri();
        when(clsEMock.getIRI()).thenReturn(IRI.create(clsEIri));
        when(clsEMock.getJavaType()).thenReturn(OWLClassE.class);
        when(etMock.getIdentifier()).thenReturn(idMock);
        when(idMock.getJavaField()).thenReturn(OWLClassK.class.getDeclaredField("uri"));
    }

    public static void initOWLClassLMocks(EntityType<OWLClassL> etMock, ListAttribute refListMock,
                                          ListAttribute simpleListMock, PluralAttribute setMock, Identifier idMock)
            throws NoSuchFieldException {
        when(etMock.getJavaType()).thenReturn(OWLClassL.class);
        when(etMock.getIRI()).thenReturn(IRI.create(OWLClassL.getClassIri()));
        when(etMock.getIdentifier()).thenReturn(idMock);
        when(idMock.getJavaField()).thenReturn(OWLClassL.class.getDeclaredField("uri"));
        when(etMock.getAttributes()).thenReturn(new HashSet<Attribute<? super OWLClassL, ?>>(Arrays.<Attribute<? super OWLClassL, ?>>asList(refListMock, simpleListMock, setMock)));

        when(refListMock.getJavaField()).thenReturn(OWLClassL.getReferencedListField());
        when(refListMock.getPersistentAttributeType()).thenReturn(PersistentAttributeType.OBJECT);
        when(refListMock.getIRI()).thenReturn(IRI.create(OWLClassL.getReferencedListField().getAnnotation(OWLObjectProperty.class).iri()));
        when(refListMock.getConstraints()).thenReturn(OWLClassL.getReferencedListField().getAnnotation(ParticipationConstraints.class).value());
        when(refListMock.getCollectionType()).thenReturn(CollectionType.LIST);
        when(refListMock.getSequenceType()).thenReturn(SequenceType.referenced);
        when(refListMock.getBindableJavaType()).thenReturn(OWLClassA.class);
        when(refListMock.isCollection()).thenReturn(true);
        when(refListMock.getOWLObjectPropertyHasNextIRI()).thenReturn(IRI.create(OWLClassL.getReferencedListField().getAnnotation(Sequence.class).ObjectPropertyHasNextIRI()));
        when(refListMock.getOWLPropertyHasContentsIRI()).thenReturn(IRI.create(OWLClassL.getReferencedListField().getAnnotation(Sequence.class).ObjectPropertyHasContentsIRI()));

        when(simpleListMock.getJavaField()).thenReturn(OWLClassL.getSimpleListField());
        when(simpleListMock.getPersistentAttributeType()).thenReturn(PersistentAttributeType.OBJECT);
        when(simpleListMock.getIRI()).thenReturn(IRI.create(OWLClassL.getSimpleListField().getAnnotation(OWLObjectProperty.class).iri()));
        when(simpleListMock.getConstraints()).thenReturn(OWLClassL.getSimpleListField().getAnnotation(ParticipationConstraints.class).value());
        when(simpleListMock.getCollectionType()).thenReturn(CollectionType.LIST);
        when(simpleListMock.getSequenceType()).thenReturn(SequenceType.simple);
        when(simpleListMock.getBindableJavaType()).thenReturn(OWLClassA.class);
        when(simpleListMock.isCollection()).thenReturn(true);
        when(simpleListMock.getOWLObjectPropertyHasNextIRI()).thenReturn(IRI.create(OWLClassL.getSimpleListField().getAnnotation(OWLObjectProperty.class).iri()));

        when(setMock.getJavaField()).thenReturn(OWLClassL.getSetField());
        when(setMock.getPersistentAttributeType()).thenReturn(PersistentAttributeType.OBJECT);
        when(setMock.getIRI()).thenReturn(IRI.create(OWLClassL.getSetField().getAnnotation(OWLObjectProperty.class).iri()));
        when(setMock.getConstraints()).thenReturn(OWLClassL.getSetField().getAnnotation(ParticipationConstraints.class).value());
        when(setMock.getCollectionType()).thenReturn(CollectionType.SET);
        when(setMock.getBindableJavaType()).thenReturn(OWLClassA.class);
        when(setMock.isCollection()).thenReturn(true);
    }
}
