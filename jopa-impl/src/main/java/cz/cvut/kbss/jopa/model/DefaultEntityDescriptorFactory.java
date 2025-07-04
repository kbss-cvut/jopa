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
 * Default implementation of the {@link EntityDescriptorFactory} interface.
 * <p>
 * It creates a new descriptor for every call.
 */
public class DefaultEntityDescriptorFactory implements EntityDescriptorFactory {

    private final MetamodelImpl metamodel;
    private final NamespaceResolver namespaceResolver;

    public DefaultEntityDescriptorFactory(MetamodelImpl metamodel, NamespaceResolver namespaceResolver) {
        this.metamodel = metamodel;
        this.namespaceResolver = namespaceResolver;
    }

    @Override
    public <T> Descriptor createDescriptor(Class<T> cls) {
        Objects.requireNonNull(cls);

        return createDescriptorImpl(cls, null, new HashMap<>());
    }

    private <T> Descriptor createDescriptorImpl(Class<T> cls, URI propagatedCtx, Map<Class<?>, Descriptor> visited) {
        if (visited.containsKey(cls)) {
            // Copy to prevent cycles in equals/hashCode of Descriptor
            return visited.get(cls).copy();
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
