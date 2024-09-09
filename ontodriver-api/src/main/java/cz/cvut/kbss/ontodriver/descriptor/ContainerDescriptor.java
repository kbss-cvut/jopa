package cz.cvut.kbss.ontodriver.descriptor;

import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.NamedResource;

import java.net.URI;

/**
 * Descriptor for reading an RDF container.
 */
public class ContainerDescriptor {

    private final Type type;

    private final NamedResource owner;

    private final Assertion property;

    private final URI context;

    protected ContainerDescriptor(Type type, NamedResource owner, Assertion property, URI context) {
        this.type = type;
        this.owner = owner;
        this.property = property;
        this.context = context;
    }

    public URI getType() {
        return type.uri;
    }

    public NamedResource getOwner() {
        return owner;
    }

    public Assertion getProperty() {
        return property;
    }

    public URI getContext() {
        return context;
    }

    /**
     * Creates a new {@link ContainerDescriptor} for reading an {@literal rdf:Alt} container from the default context.
     *
     * @param owner    Container owner
     * @param property Property referencing the container
     * @return Container descriptor
     */
    public static ContainerDescriptor altDescriptor(NamedResource owner, Assertion property) {
        return new ContainerDescriptor(Type.ALT, owner, property, null);
    }

    /**
     * Creates a new {@link ContainerDescriptor} for reading an {@literal rdf:Alt} container from the specified
     * context.
     *
     * @param owner    Container owner
     * @param property Property referencing the container
     * @param context  Context from which the container should be read
     * @return Container descriptor
     */
    public static ContainerDescriptor altDescriptor(NamedResource owner, Assertion property, URI context) {
        return new ContainerDescriptor(Type.ALT, owner, property, context);
    }

    /**
     * Creates a new {@link ContainerDescriptor} for reading an {@literal rdf:Bag} container from the default context.
     *
     * @param owner    Container owner
     * @param property Property referencing the container
     * @return Container descriptor
     */
    public static ContainerDescriptor bagDescriptor(NamedResource owner, Assertion property) {
        return new ContainerDescriptor(Type.BAG, owner, property, null);
    }

    /**
     * Creates a new {@link ContainerDescriptor} for reading an {@literal rdf:Bag} container from the specified
     * context.
     *
     * @param owner    Container owner
     * @param property Property referencing the container
     * @param context  Context from which the container should be read
     * @return Container descriptor
     */
    public static ContainerDescriptor bagDescriptor(NamedResource owner, Assertion property, URI context) {
        return new ContainerDescriptor(Type.BAG, owner, property, context);
    }

    /**
     * Creates a new {@link ContainerDescriptor} for reading an {@literal rdf:Seq} container from the default context.
     *
     * @param owner    Container owner
     * @param property Property referencing the container
     * @return Container descriptor
     */
    public static ContainerDescriptor seqDescriptor(NamedResource owner, Assertion property) {
        return new ContainerDescriptor(Type.SEQ, owner, property, null);
    }

    /**
     * Creates a new {@link ContainerDescriptor} for reading an {@literal rdf:Seq} container from the specified
     * context.
     *
     * @param owner    Container owner
     * @param property Property referencing the container
     * @param context  Context from which the container should be read
     * @return Container descriptor
     */
    public static ContainerDescriptor seqDescriptor(NamedResource owner, Assertion property, URI context) {
        return new ContainerDescriptor(Type.SEQ, owner, property, context);
    }

    protected enum Type {

        ALT(URI.create("http://www.w3.org/1999/02/22-rdf-syntax-ns#Alt")),
        BAG(URI.create("http://www.w3.org/1999/02/22-rdf-syntax-ns#Bag")),
        SEQ(URI.create("http://www.w3.org/1999/02/22-rdf-syntax-ns#Seq"));

        private final URI uri;

        Type(URI uri) {
            this.uri = uri;
        }
    }
}
