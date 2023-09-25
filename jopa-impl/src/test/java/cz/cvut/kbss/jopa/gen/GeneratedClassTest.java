package cz.cvut.kbss.jopa.gen;

import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.OWLClassI;
import cz.cvut.kbss.jopa.environment.utils.MetamodelMocks;
import cz.cvut.kbss.jopa.model.Manageable;
import cz.cvut.kbss.jopa.model.MetamodelImpl;
import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import cz.cvut.kbss.jopa.model.metamodel.AnnotatedAccessor;
import cz.cvut.kbss.jopa.sessions.UnitOfWorkImpl;
import net.bytebuddy.ByteBuddy;
import net.bytebuddy.NamingStrategy;
import net.bytebuddy.description.modifier.FieldPersistence;
import net.bytebuddy.description.modifier.Visibility;
import net.bytebuddy.description.type.TypeDescription;
import net.bytebuddy.dynamic.DynamicType;
import net.bytebuddy.implementation.FieldAccessor;
import net.bytebuddy.implementation.MethodDelegation;
import net.bytebuddy.implementation.SuperMethodCall;
import net.bytebuddy.implementation.bind.annotation.Origin;
import net.bytebuddy.implementation.bind.annotation.This;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import java.io.File;
import java.lang.reflect.Method;

import static net.bytebuddy.matcher.ElementMatchers.isGetter;
import static net.bytebuddy.matcher.ElementMatchers.isSetter;
import static net.bytebuddy.matcher.ElementMatchers.named;
import static net.bytebuddy.matcher.ElementMatchers.not;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * POC generating classes
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public class GeneratedClassTest {

    @Mock
    private UnitOfWorkImpl uow;

    @BeforeEach
    void setUp() throws Exception {
        final MetamodelMocks metamodelMocks = new MetamodelMocks();
        final MetamodelImpl mm = mock(MetamodelImpl.class);
        metamodelMocks.setMocks(mm);
        when(uow.getMetamodel()).thenReturn(mm);
    }

    @Test
    void generateClassHasParentTypeAnnotations() throws Exception {
        final Class<? extends OWLClassA> cls = generateSubclass(OWLClassA.class);
        assertNotNull(cls.getAnnotation(OWLClass.class));
        assertEquals(OWLClassA.class.getAnnotation(OWLClass.class).iri(), cls.getAnnotation(OWLClass.class).iri());
    }

    @Test
    void generatedSetterInvokesPersistenceContextAttributeChangeHandler() throws Exception {
        final Class<? extends OWLClassA> cls = generateSubclass(OWLClassA.class);

        final OWLClassA instance = cls.getDeclaredConstructor().newInstance();
        assertInstanceOf(Manageable.class, instance);
        assertInstanceOf(OWLClassA.class, instance);
        ((Manageable) instance).setPersistenceContext(uow);
        assertEquals(uow, ((Manageable) instance).getPersistenceContext());
        instance.setStringAttribute("test value");
        assertEquals("test value", instance.getStringAttribute());
        verify(uow).attributeChanged(instance, OWLClassA.getStrAttField());
    }

    @Test
    void generatedGetterInvokesPersistenceContextAttributeLoadForLazyAttribute() throws Exception {
        final Class<? extends OWLClassI> cls = generateSubclass(OWLClassI.class);

        final OWLClassI instance = cls.getDeclaredConstructor().newInstance();
        assertInstanceOf(Manageable.class, instance);
        assertInstanceOf(OWLClassI.class, instance);
        ((Manageable) instance).setPersistenceContext(uow);
        assertEquals(uow, ((Manageable) instance).getPersistenceContext());
        assertNull(instance.getOwlClassA());
        verify(uow).loadEntityField(instance, OWLClassI.getOwlClassAField());
    }

    private <T> Class<? extends T> generateSubclass(Class<T> entityClass) throws Exception {
        final ByteBuddy bb = new ByteBuddy().with(new NamingStrategy.AbstractBase() {
            @Override
            protected String name(TypeDescription typeDescription) {
                return "JOPA_" + typeDescription.getSimpleName();
            }
        });
        DynamicType.Unloaded<? extends T> typeDef = bb.subclass(entityClass)
                                                      .annotateType(entityClass.getAnnotations())
                                                      .defineField("persistenceContext", UnitOfWorkImpl.class, Visibility.PRIVATE, FieldPersistence.TRANSIENT)
                                                      .implement(Manageable.class)
                                                      .intercept(FieldAccessor.ofBeanProperty())
                                                      .method(isSetter().and(not(named("setPersistenceContext"))))
                                                      .intercept(SuperMethodCall.INSTANCE.andThen(MethodDelegation.to(SetterInterceptor.class)))
                                                      .method(isGetter().and(not(named("getPersistenceContext"))))
                                                      .intercept(MethodDelegation.to(GetterInterceptor.class)
                                                                                 .andThen(SuperMethodCall.INSTANCE))
                                                      .make();

        final Class<? extends T> result = typeDef.load(getClass().getClassLoader()).getLoaded();
//        typeDef.saveIn(new File(result.getSimpleName()));
        return result;
    }

    public static class SetterInterceptor {

        public static void set(@This Manageable instance, @Origin Method setter) throws Exception {
            if (instance.getPersistenceContext() == null) {
                return;
            }
            final String fieldName = AnnotatedAccessor.from(setter).getPropertyName();
//            final EntityType<?> et = instance.getPersistenceContext().getMetamodel().entity(instance.getClass());
//            instance.getPersistenceContext()
//                    .attributeChanged(instance, et.getFieldSpecification(fieldName));
            // TODO UnitOfWork.attributeChanged should take FieldSpecification instead of Field
            instance.getPersistenceContext()
                    .attributeChanged(instance, setter.getDeclaringClass().getDeclaredField(fieldName));
        }
    }

    public static class GetterInterceptor {

        public static void get(@This Manageable instance, @Origin Method getter) throws Exception {
            if (instance.getPersistenceContext() == null) {
                return;
            }
            final String fieldName = AnnotatedAccessor.from(getter).getPropertyName();
//             final EntityType<?> et = instance.getPersistenceContext().getMetamodel().entity(instance.getClass());
//            instance.getPersistenceContext()
//                    .loadEntityField(instance, et.getFieldSpecification(fieldName));
            // TODO UnitOfWork.loadEntityField should take FieldSpecification instead of Field
            instance.getPersistenceContext()
                    .loadEntityField(instance, getter.getDeclaringClass().getDeclaredField(fieldName));
        }
    }
}
