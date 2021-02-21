package cz.cvut.kbss.jopa.model.metamodel;

import cz.cvut.kbss.jopa.exception.StaticMetamodelInitializationException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.util.Objects;
import java.util.Optional;

/**
 * Initializes static metamodel based on the provided runtime metamodel.
 * <p>
 * Static metamodel initialization involves going through all managed types in the metamodel, finding a corresponding static metamodel class (if exists)
 * and populating its attributes with values from the actual runtime metamodel.
 */
public class StaticMetamodelInitializer {

    private static final Logger LOG = LoggerFactory.getLogger(StaticMetamodelInitializer.class);

    /**
     * Suffix appended to a static metamodel class corresponding to a metamodel class.
     */
    private static final String STATIC_METAMODEL_CLASS_SUFFIX = "_";

    private final Metamodel metamodel;

    public StaticMetamodelInitializer(Metamodel metamodel) {
        this.metamodel = metamodel;
    }

    /**
     * Executes the static metamodel initialization.
     */
    public void initializeStaticMetamodel() {
        LOG.debug("Initializing static metamodel.");
        processEntities();
        // TODO process mapped superclasses
    }

    private void processEntities() {
        metamodel.getEntities().forEach(et -> {
            final Optional<Class<?>> smClass = tryFindingClass(et);
            if (!smClass.isPresent()) {
                LOG.trace("No static metamodel type found for {}.", et);
                return;
            }
            LOG.debug("Processing static metamodel class {} corresponding to {}.", smClass.get(), et);
            try {
                initStaticMembers(et, smClass.get());
            } catch (IllegalAccessException e) {
                throw new StaticMetamodelInitializationException("Unable to initialize static metamodel class " + smClass, e);
            }
        });
    }

    private Optional<Class<?>> tryFindingClass(EntityType<?> et) {
        final String staticName = et.getJavaType().getName() + STATIC_METAMODEL_CLASS_SUFFIX;
        try {
            final Class<?> smClass = Class.forName(staticName);
            if (isNotStaticMetamodelForType(smClass, et.getJavaType())) {
                return Optional.empty();
            }
            return Optional.of(smClass);
        } catch (ClassNotFoundException e) {
            // Swallow the exception, this just means there is no static metamodel type for the specified entity
            return Optional.empty();
        }
    }

    private static boolean isNotStaticMetamodelForType(Class<?> smClass, Class<?> metamodelClass) {
        return smClass.getAnnotation(StaticMetamodel.class) == null || !smClass.getAnnotation(StaticMetamodel.class).value().equals(metamodelClass);
    }

    private <T> void initStaticMembers(EntityType<T> et, Class<?> smClass) throws IllegalAccessException {
        final Field[] fields = smClass.getDeclaredFields();
        for (Field f : fields) {
            final FieldSpecification<T, ?> att = getMetamodelMember(f, et);
            setFieldValue(f, att);
        }
    }

    private static void setFieldValue(Field field, Object value) throws IllegalAccessException {
        if (!Modifier.isStatic(field.getModifiers()) || !Modifier.isPublic(field.getModifiers())) {
            throw new StaticMetamodelInitializationException("Static metamodel field " + field + " must be public static.");
        }
        field.set(null, value);
    }

    private <T> FieldSpecification<T, ?> getMetamodelMember(Field field, EntityType<T> et) {
        LOG.trace("Finding metamodel member for static metamodel field {}.", field);
        return getDeclaredIdentifier(field, et)
                .orElseGet(() -> getDeclaredAttribute(field, et)
                        .orElseGet(() -> getDeclaredTypes(field, et)
                                .orElseGet(() -> getDeclaredProperties(field, et)
                                        .orElseThrow(() -> new StaticMetamodelInitializationException("No corresponding metamodel member found for static metamodel field " + field)))));
    }

    private <T> Optional<FieldSpecification<T, ?>> getDeclaredIdentifier(Field field, EntityType<T> et) {
        return Objects.equals(field.getName(), et.getIdentifier().getJavaField().getName()) && isDeclaredInClass(et.getIdentifier(), et.getJavaType()) ? Optional.of((Identifier<T, ?>) et.getIdentifier()) : Optional.empty();
    }

    private boolean isDeclaredInClass(FieldSpecification<?, ?> fs, Class<?> cls) {
        return fs.getJavaField().getDeclaringClass().equals(cls);
    }

    private <T> Optional<FieldSpecification<T, ?>> getDeclaredAttribute(Field field, EntityType<T> et) {
        try {
            return Optional.of(et.getDeclaredAttribute(field.getName()));
        } catch (IllegalArgumentException e) {
            return Optional.empty();
        }
    }

    private <T> Optional<FieldSpecification<T, ?>> getDeclaredTypes(Field field, EntityType<T> et) {
        return et.getTypes() != null && Objects.equals(field.getName(), et.getTypes().getJavaField().getName()) && isDeclaredInClass(et.getTypes(), et.getJavaType()) ? Optional.of((TypesSpecification<T, ?>) et.getTypes()) : Optional.empty();
    }

    private <T> Optional<FieldSpecification<T, ?>> getDeclaredProperties(Field field, EntityType<T> et) {
        return et.getProperties() != null && Objects.equals(field.getName(), et.getProperties().getJavaField().getName()) && isDeclaredInClass(et.getProperties(), et.getJavaType()) ? Optional.of((PropertiesSpecification<T, ?, ?, ?>) et.getProperties()) : Optional.empty();
    }
}
