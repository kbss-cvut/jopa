package cz.cvut.kbss.jopa.model;

import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.Vocabulary;
import cz.cvut.kbss.jopa.environment.utils.TestLocal;
import cz.cvut.kbss.jopa.model.annotations.Context;
import cz.cvut.kbss.jopa.model.annotations.Id;
import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import cz.cvut.kbss.jopa.model.annotations.OWLDataProperty;
import cz.cvut.kbss.jopa.model.annotations.OWLObjectProperty;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.model.descriptors.FieldDescriptor;
import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.model.metamodel.IdentifiableEntityType;
import cz.cvut.kbss.jopa.model.metamodel.PluralAttribute;
import cz.cvut.kbss.jopa.utils.NamespaceResolver;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Spy;
import org.mockito.junit.jupiter.MockitoExtension;

import java.net.URI;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

@SuppressWarnings({"unused", "unchecked"})
@ExtendWith(MockitoExtension.class)
class EntityDescriptorFactoryTest {

    @Mock
    private MetamodelImpl metamodel;

    @Spy
    private NamespaceResolver namespaceResolver = new NamespaceResolver();

    @InjectMocks
    private EntityDescriptorFactory sut;

    @Test
    void createDescriptorCreatesEntityDescriptorWithContextSpecifiedInAnnotationOnEntityClass() throws Exception {
        final IdentifiableEntityType<SimpleWithContext> type = mock(IdentifiableEntityType.class);
        when(metamodel.entity(SimpleWithContext.class)).thenReturn(type);
        final Attribute<SimpleWithContext, String> att = mock(Attribute.class);
        when(type.getAttributes()).thenReturn(Set.of(att));
        when(att.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.DATA);
        when(att.getJavaMember()).thenReturn(SimpleWithContext.class.getDeclaredField("stringAttribute"));

        final Descriptor descriptor = sut.createDescriptor(SimpleWithContext.class);
        assertInstanceOf(EntityDescriptor.class, descriptor);
        assertEquals(Set.of(URI.create("https://example.com/context")), descriptor.getContexts());
        assertEquals(Set.of(URI.create("https://example.com/context")), descriptor.getAttributeContexts(att));
    }

    @TestLocal
    @Context("https://example.com/context")
    @OWLClass(iri = Vocabulary.CLASS_BASE + "simpleWithContext")
    private static class SimpleWithContext {
        @Id
        private URI id;

        @OWLDataProperty(iri = Vocabulary.ATTRIBUTE_BASE + "stringAttribute")
        private String stringAttribute;
    }

    @Test
    void createDescriptorCreatesDescriptorUsingNamespace() {
        namespaceResolver.registerNamespace("ex", "https://example.com/");
        final IdentifiableEntityType<SimpleWithNamespacedContext> type = mock(IdentifiableEntityType.class);
        when(metamodel.entity(SimpleWithNamespacedContext.class)).thenReturn(type);
        final Attribute<SimpleWithNamespacedContext, String> att = mock(Attribute.class);

        final Descriptor result = sut.createDescriptor(SimpleWithNamespacedContext.class);
        assertInstanceOf(EntityDescriptor.class, result);
        assertEquals(Set.of(URI.create("https://example.com/context")), result.getContexts());
        assertEquals(Set.of(URI.create("https://example.com/context")), result.getAttributeContexts(att));
    }

    @TestLocal
    @Context("ex:context")
    @OWLClass(iri = Vocabulary.CLASS_BASE + "simpleWithNamespacedContext")
    private static class SimpleWithNamespacedContext {
        @Id
        private URI id;

        @OWLDataProperty(iri = Vocabulary.ATTRIBUTE_BASE + "stringAttribute")
        private String stringAttribute;
    }

    @Test
    void createDescriptorTraversesAssociationRelationshipAndRecursivelyCreatesDescriptor() throws Exception {
        namespaceResolver.registerNamespace("ex", "https://example.com/");
        final IdentifiableEntityType<WithReference> rootType = mock(IdentifiableEntityType.class);
        final IdentifiableEntityType<SimpleWithNamespacedContext> referencedType = mock(IdentifiableEntityType.class);
        final Attribute<WithReference, SimpleWithNamespacedContext> att = mock(Attribute.class);
        when(att.getJavaType()).thenReturn(SimpleWithNamespacedContext.class);
        when(att.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.OBJECT);
        when(att.getJavaMember()).thenReturn(WithReference.class.getDeclaredField("reference"));
        when(rootType.getAttributes()).thenReturn(Set.of(att));
        when(metamodel.entity(WithReference.class)).thenReturn(rootType);
        when(metamodel.entity(SimpleWithNamespacedContext.class)).thenReturn(referencedType);

        final Descriptor resul = sut.createDescriptor(WithReference.class);
        assertInstanceOf(EntityDescriptor.class, resul);
        assertEquals(Set.of(URI.create("https://example.com/another-context")), resul.getContexts());
        assertEquals(Set.of(URI.create("https://example.com/another-context")), resul.getAttributeContexts(att));
        final Descriptor referenceDescriptor = resul.getAttributeDescriptor(att);
        assertInstanceOf(EntityDescriptor.class, referenceDescriptor);
        assertEquals(Set.of(URI.create("https://example.com/context")), referenceDescriptor.getContexts());
    }

    @TestLocal
    @Context("ex:another-context")
    @OWLClass(iri = Vocabulary.CLASS_BASE + "withReference")
    private static class WithReference {
        @Id
        private URI id;

        @OWLObjectProperty(iri = Vocabulary.ATTRIBUTE_BASE + "reference")
        private SimpleWithNamespacedContext reference;
    }

    @Test
    void createDescriptorRespectsContextDefinedForDataPropertyAttribute() throws Exception {
        final IdentifiableEntityType<WithAttributeContexts> type = mock(IdentifiableEntityType.class);
        when(metamodel.entity(WithAttributeContexts.class)).thenReturn(type);
        final Attribute<WithAttributeContexts, String> att = mock(Attribute.class);
        when(type.getAttributes()).thenReturn(Set.of(att));
        when(att.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.DATA);
        when(att.getJavaMember()).thenReturn(WithAttributeContexts.class.getDeclaredField("stringAttribute"));

        final Descriptor result = sut.createDescriptor(WithAttributeContexts.class);
        assertInstanceOf(EntityDescriptor.class, result);
        assertEquals(Set.of(), result.getContexts());
        assertEquals(Set.of(URI.create("https://example.com/context")), result.getAttributeContexts(att));
    }

    @TestLocal
    @OWLClass(iri = Vocabulary.CLASS_BASE + "WithAttributeContexts")
    private static class WithAttributeContexts {
        @Id
        private URI id;

        @Context("https://example.com/context")
        @OWLDataProperty(iri = Vocabulary.ATTRIBUTE_BASE + "stringAttribute")
        private String stringAttribute;

        @Context("https://example.com/reference-context")
        @OWLObjectProperty(iri = Vocabulary.ATTRIBUTE_BASE + "reference")
        private OWLClassA reference;
    }

    @Test
    void createDescriptorPropagatesContextToReferencedClassWhenConfiguredAndReferenceDoesNotHaveContext() throws Exception {
        namespaceResolver.registerNamespace("ex", "https://example.com/");
        final IdentifiableEntityType<PropagatesContext> type = mock(IdentifiableEntityType.class);
        when(metamodel.entity(PropagatesContext.class)).thenReturn(type);
        final Attribute<PropagatesContext, OWLClassA> att = mock(Attribute.class);
        when(type.getAttributes()).thenReturn(Set.of(att));
        when(att.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.OBJECT);
        when(att.getJavaType()).thenReturn(OWLClassA.class);
        when(att.getJavaMember()).thenReturn(PropagatesContext.class.getDeclaredField("reference"));
        final IdentifiableEntityType<OWLClassA> referencedType = mock(IdentifiableEntityType.class);
        when(metamodel.entity(OWLClassA.class)).thenReturn(referencedType);

        final Descriptor result = sut.createDescriptor(PropagatesContext.class);
        assertInstanceOf(EntityDescriptor.class, result);
        assertEquals(Set.of(URI.create("https://example.com/context")), result.getContexts());
        final Descriptor referenceDescriptor = result.getAttributeDescriptor(att);
        assertInstanceOf(EntityDescriptor.class, referenceDescriptor);
        assertEquals(Set.of(URI.create("https://example.com/context")), referenceDescriptor.getContexts());
    }

    @TestLocal
    @Context(value = "ex:context", propagate = true)
    @OWLClass(iri = Vocabulary.CLASS_BASE + "PropagatesContext")
    private static class PropagatesContext {
        @Id
        private URI id;

        @OWLObjectProperty(iri = Vocabulary.ATTRIBUTE_BASE + "reference")
        private OWLClassA reference;
    }

    @Test
    void createDescriptorPropagatesContextDefinedOnAttributeWhenItIsObjectProperty() throws Exception {
        final IdentifiableEntityType<WithAttributeContexts> type = mock(IdentifiableEntityType.class);
        final IdentifiableEntityType<OWLClassA> referencedType = mock(IdentifiableEntityType.class);
        when(metamodel.entity(WithAttributeContexts.class)).thenReturn(type);
        final Attribute<WithAttributeContexts, OWLClassA> att = mock(Attribute.class);
        when(type.getAttributes()).thenReturn(Set.of(att));
        when(att.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.OBJECT);
        when(att.getJavaMember()).thenReturn(WithAttributeContexts.class.getDeclaredField("reference"));
        when(att.getJavaType()).thenReturn(OWLClassA.class);
        when(metamodel.entity(OWLClassA.class)).thenReturn(referencedType);

        final Descriptor result = sut.createDescriptor(WithAttributeContexts.class);
        assertInstanceOf(EntityDescriptor.class, result);
        assertEquals(Set.of(), result.getContexts());
        final Descriptor referenceDescriptor = result.getAttributeDescriptor(att);
        assertInstanceOf(EntityDescriptor.class, referenceDescriptor);
        assertEquals(Set.of(URI.create("https://example.com/reference-context")), referenceDescriptor.getContexts());
    }

    @Test
    void createDescriptorCorrectlyResolvesTargetTypeOfPluralObjectProperty() throws Exception {
        namespaceResolver.registerNamespace("ex", "https://example.com/");
        final IdentifiableEntityType<WithPluralReference> rootType = mock(IdentifiableEntityType.class);
        final IdentifiableEntityType<SimpleWithNamespacedContext> referencedType = mock(IdentifiableEntityType.class);
        final PluralAttribute<WithPluralReference, Set, SimpleWithNamespacedContext> att = mock(PluralAttribute.class);
        when(att.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.OBJECT);
        when(att.getBindableJavaType()).thenReturn(SimpleWithNamespacedContext.class);
        when(att.isCollection()).thenReturn(true);
        when(att.getJavaMember()).thenReturn(WithPluralReference.class.getDeclaredField("reference"));
        when(rootType.getAttributes()).thenReturn(Set.of(att));
        when(metamodel.entity(WithPluralReference.class)).thenReturn(rootType);
        when(metamodel.entity(SimpleWithNamespacedContext.class)).thenReturn(referencedType);

        final Descriptor result = sut.createDescriptor(WithPluralReference.class);
        assertInstanceOf(EntityDescriptor.class, result);
        assertEquals(Set.of(URI.create("https://example.com/another-context")), result.getContexts());
        final Descriptor referenceDescriptor = result.getAttributeDescriptor(att);
        assertInstanceOf(EntityDescriptor.class, referenceDescriptor);
        assertEquals(Set.of(URI.create("https://example.com/context")), referenceDescriptor.getContexts());
    }

    @TestLocal
    @Context("ex:another-context")
    @OWLClass(iri = Vocabulary.CLASS_BASE + "WithPluralReference")
    private static class WithPluralReference {
        @Id
        private URI id;

        @OWLObjectProperty(iri = Vocabulary.ATTRIBUTE_BASE + "reference")
        private Set<SimpleWithNamespacedContext> reference;
    }

    @Test
    void createDescriptorDoesNotAttemptToTraverseObjectPropertyAttributeWhenValueTypeIsIdentifier() throws Exception {
        final IdentifiableEntityType<WithIdentifierReference> type = mock(IdentifiableEntityType.class);
        when(metamodel.entity(WithIdentifierReference.class)).thenReturn(type);
        final Attribute<WithIdentifierReference, URI> att = mock(Attribute.class);
        when(type.getAttributes()).thenReturn(Set.of(att));
        when(att.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.OBJECT);
        when(att.getJavaType()).thenReturn(URI.class);
        when(att.getJavaMember()).thenReturn(WithIdentifierReference.class.getDeclaredField("reference"));

        final Descriptor result = sut.createDescriptor(WithIdentifierReference.class);
        assertInstanceOf(EntityDescriptor.class, result);
        assertEquals(Set.of(URI.create("https://example.com/context")), result.getContexts());
        final Descriptor referenceDescriptor = result.getAttributeDescriptor(att);
        assertInstanceOf(FieldDescriptor.class, referenceDescriptor);
        assertEquals(Set.of(URI.create("https://example.com/context")), referenceDescriptor.getContexts());
    }

    @TestLocal
    @Context("https://example.com/context")
    @OWLClass(iri = Vocabulary.CLASS_BASE + "WithIdentifierReference")
    private static class WithIdentifierReference {
        @Id
        private URI id;

        @OWLObjectProperty(iri = Vocabulary.ATTRIBUTE_BASE + "reference")
        private URI reference;
    }

    @Test
    void createDescriptorHandlesReferenceCycles() throws Exception {
        final IdentifiableEntityType<WithReferenceCycle> type = mock(IdentifiableEntityType.class);
        when(metamodel.entity(WithReferenceCycle.class)).thenReturn(type);
        final Attribute<WithReferenceCycle, WithReferenceCycle> att = mock(Attribute.class);
        when(type.getAttributes()).thenReturn(Set.of(att));
        when(att.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.OBJECT);
        when(att.getJavaType()).thenReturn(WithReferenceCycle.class);
        when(att.getJavaMember()).thenReturn(WithReferenceCycle.class.getDeclaredField("reference"));

        final Descriptor result = sut.createDescriptor(WithReferenceCycle.class);
        assertInstanceOf(EntityDescriptor.class, result);
        assertEquals(Set.of(URI.create("https://example.com/context")), result.getContexts());
        final Descriptor referenceDescriptor = result.getAttributeDescriptor(att);
        assertEquals(result, result.getAttributeDescriptor(att));
    }

    @TestLocal
    @Context("https://example.com/context")
    @OWLClass(iri = Vocabulary.CLASS_BASE + "WithReferenceCycle")
    private static class WithReferenceCycle {
        @Id
        private URI id;

        @OWLObjectProperty(iri = Vocabulary.ATTRIBUTE_BASE + "reference")
        private WithReferenceCycle reference;
    }
}
