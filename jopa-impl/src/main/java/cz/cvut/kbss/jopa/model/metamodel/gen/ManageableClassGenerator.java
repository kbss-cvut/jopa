package cz.cvut.kbss.jopa.model.metamodel.gen;

import cz.cvut.kbss.jopa.model.JOPAPersistenceProperties;
import cz.cvut.kbss.jopa.model.Manageable;
import cz.cvut.kbss.jopa.model.metamodel.AnnotatedAccessor;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.jopa.sessions.UnitOfWork;
import cz.cvut.kbss.jopa.sessions.validator.AttributeModificationValidator;
import cz.cvut.kbss.jopa.utils.Configuration;
import cz.cvut.kbss.jopa.utils.EntityPropertiesUtils;
import cz.cvut.kbss.jopa.utils.MetamodelUtils;
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
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.IOException;
import java.lang.reflect.Method;
import java.util.Objects;

import static net.bytebuddy.matcher.ElementMatchers.isSetter;

/**
 * Generates persistence context-aware classes that implement the {@link cz.cvut.kbss.jopa.model.Manageable} interface.
 * <p>
 * Such classes have an additional attribute not inherited from the base entity class. This attribute's value is a
 * reference to the persistence context to which an instance is attached. {@link cz.cvut.kbss.jopa.model.Manageable}
 * allows establishing and accessing this connection.
 */
public class ManageableClassGenerator implements PersistenceContextAwareClassGenerator {

    private static final Logger LOG = LoggerFactory.getLogger(ManageableClassGenerator.class);

    private final ByteBuddy byteBuddy = new ByteBuddy().with(new NamingStrategy.AbstractBase() {

        @Override
        protected String name(TypeDescription typeDescription) {
            return "JOPA_" + typeDescription.getSimpleName();
        }
    });

    private final Configuration config;

    public ManageableClassGenerator(Configuration config) {this.config = config;}

    @Override
    public <T> Class<? extends T> generate(Class<T> entityClass) {
        Objects.requireNonNull(entityClass);
        LOG.trace("Generating dynamic type for entity class {}.", entityClass);
        DynamicType.Unloaded<? extends T> typeDef = byteBuddy.subclass(entityClass)
                                                             .annotateType(entityClass.getAnnotations())
                                                             .annotateType(new GeneratedEntityClassImpl())
                                                             .defineField("persistenceContext", UnitOfWork.class, Visibility.PRIVATE, FieldPersistence.TRANSIENT)
                                                             .implement(Manageable.class)
                                                             .intercept(FieldAccessor.ofBeanProperty())
                                                             .method(isSetter().and(new PersistentPropertySetterMatcher<>(entityClass)))
                                                             .intercept(SuperMethodCall.INSTANCE.andThen(MethodDelegation.to(SetterInterceptor.class)))
                                                             .make();
        LOG.debug("Generated dynamic type {} for entity class {}.", typeDef, entityClass);
        outputGeneratedClass(typeDef);
        return typeDef.load(getClass().getClassLoader()).getLoaded();
    }

    private <T> void outputGeneratedClass(DynamicType.Unloaded<? extends T> typeDef) {
        final String outputDir = config.get(JOPAPersistenceProperties.CLASS_GENERATOR_OUTPUT_DIR, "");
        if (!outputDir.isBlank()) {
            final File output = new File(outputDir);
            try {
                LOG.trace("Saving generated class '{}' to '{}'.", typeDef, output);
                typeDef.saveIn(output);
            } catch (IOException e) {
                LOG.error("Unable to output generated class {} to file {}.", typeDef, output, e);
            }
        }
    }

    public static class SetterInterceptor {

        private SetterInterceptor() {
            throw new AssertionError();
        }

        public static void set(@This Manageable instance, @Origin Method setter) {
            final UnitOfWork pc = instance.getPersistenceContext();
            if (pc == null || !pc.isInTransaction()) {
                return;
            }
            final String fieldName = AnnotatedAccessor.from(setter).getPropertyName();
            final EntityType<?> et = pc.getMetamodel().entity(MetamodelUtils.getEntityClass(instance.getClass()));
            final FieldSpecification<?, ?> fieldSpec = et.getFieldSpecification(fieldName);
            if (EntityPropertiesUtils.isFieldTransient(fieldSpec.getJavaField())) {
                return;
            }
            AttributeModificationValidator.verifyCanModify(fieldSpec);
            pc.attributeChanged(instance, fieldSpec);
        }
    }
}
