package cz.cvut.kbss.jopa.model;

import cz.cvut.kbss.jopa.model.annotations.Context;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.PluralAttribute;
import cz.cvut.kbss.jopa.utils.NamespaceResolver;

import java.lang.reflect.Field;
import java.lang.reflect.Member;
import java.lang.reflect.Method;
import java.net.URI;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;

/**
 * Creates entity descriptors based on metamodel information.
 * <p>
 * The logic is based on the following rules:
 * <ul>
 *     <li>If a class has context declared, use it for all its fields</li>
 *     <li>If a field has context declared, and it is an object property referencing another entity, propagate the context to that entity</li>
 *     <li>If a class does not have context declared, use context propagated to its from referencing class (if available)</li>
 *     <li>If a field has context and is not an object property referencing another entity, use the context for the value</li>
 * </ul>
 */
public class EntityDescriptorFactory {

    private final MetamodelImpl metamodel;
    private final NamespaceResolver namespaceResolver;

    public EntityDescriptorFactory(MetamodelImpl metamodel, NamespaceResolver namespaceResolver) {
        this.metamodel = metamodel;
        this.namespaceResolver = namespaceResolver;
    }

    /**
     * Creates a descriptor for the specified entity class.
     *
     * @param cls Entity class to generate descriptor for
     * @param <T> Entity type
     * @return Entity descriptor
     * @throws IllegalArgumentException If the specified class is not an entity class
     */
    public <T> Descriptor createDescriptor(Class<T> cls) {
        Objects.requireNonNull(cls);

        return createDescriptorImpl(cls, null, new HashMap<>());
    }

    private <T> Descriptor createDescriptorImpl(Class<T> cls, URI propagatedCtx, Map<Class<?>, Descriptor> visited) {
        if (visited.containsKey(cls)) {
            return visited.get(cls);
        }
        final Optional<Context> clsContext = resolveContext(cls);
        final EntityDescriptor result = clsContext.map(ctx -> new EntityDescriptor(Set.of(contextUri(ctx))))
                                                  .orElseGet(() -> propagatedCtx != null ? new EntityDescriptor(Set.of(propagatedCtx)) : new EntityDescriptor());
        visited.put(cls, result);

        final EntityType<T> entityType = metamodel.entity(cls);
        entityType.getAttributes().forEach(att -> {
            final Optional<Context> attributeContext = resolveContext(att.getJavaMember());
            if (att.getPersistentAttributeType() == Attribute.PersistentAttributeType.OBJECT) {
                final Class<?> javaType = att.isCollection() ? ((PluralAttribute<? super T, ?, ?>) att).getBindableJavaType() : att.getJavaType();
                if (!metamodel.isEntityType(javaType)) {
                    attributeContext.ifPresent(ctx -> result.addAttributeContext(att, contextUri(ctx)));
                } else {
                    result.addAttributeDescriptor(att, createDescriptorImpl(javaType,
                            resolveContextPropagation(clsContext.orElse(null), attributeContext.orElse(null)), visited));
                }
            } else {
                attributeContext.ifPresent(ctx -> result.addAttributeContext(att, contextUri(ctx)));
            }
        });
        if (entityType.getTypes() != null) {
            final Optional<Context> typesCtx = resolveContext(entityType.getTypes().getJavaField());
            typesCtx.ifPresent(context -> result.addAttributeContext(entityType.getTypes(), contextUri(context)));
        }
        if (entityType.getProperties() != null) {
            final Optional<Context> propertiesCtx = resolveContext(entityType.getProperties().getJavaField());
            propertiesCtx.ifPresent(context -> result.addAttributeContext(entityType.getProperties(), contextUri(context)));
        }
        return result;
    }

    private Optional<Context> resolveContext(Class<?> cls) {
        final Context ctx = cls.getAnnotation(Context.class);
        return Optional.ofNullable(ctx);
    }

    private Optional<Context> resolveContext(Member member) {
        if (member instanceof Method m) {
            return Optional.ofNullable(m.getAnnotation(Context.class));
        } else {
            assert member instanceof Field;
            return Optional.ofNullable(((Field) member).getAnnotation(Context.class));
        }
    }

    private URI contextUri(Context ctx) {
        return URI.create(namespaceResolver.resolveFullIri(ctx.value()));
    }

    private URI resolveContextPropagation(Context clsContext, Context attContext) {
        if (attContext != null) {
            return contextUri(attContext);
        } else if (clsContext != null && clsContext.propagate()) {
            return contextUri(clsContext);
        }
        return null;
    }
}
