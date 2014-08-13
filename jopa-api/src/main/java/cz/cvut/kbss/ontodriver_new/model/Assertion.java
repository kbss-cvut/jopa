package cz.cvut.kbss.ontodriver_new.model;

import java.net.URI;

/**
 * Base assertion axiom class. </p>
 * 
 * Defines just whether the assertion uses inferred values and existing types of
 * assertions. </p>
 * 
 * The usage of types may seem as not being very object-oriented, but since the
 * hierarchy is fixed (there aren't any other kinds of assertions in ontologies)
 * and since the subclasses essentially don't contain any behavior, we can use
 * this way.
 * 
 * @author ledvima1
 * 
 */
public abstract class Assertion extends NamedResource {

	private static final long serialVersionUID = 2641835840569464452L;

	private final boolean inferred;

	public static enum AssertionType {
		CLASS, OBJECT_PROPERTY, DATA_PROPERTY, ANNOTATION_PROPERTY
	}

	protected Assertion(URI identifier, boolean isInferred) {
		super(identifier);
		this.inferred = isInferred;
	}

	/**
	 * Whether this assertion is based on inferred values.
	 * 
	 * @return True if inferred, false otherwise
	 */
	public boolean isInferred() {
		return inferred;
	}

	/**
	 * Gets type of this assertion.
	 * 
	 * @return Assertion type
	 */
	public abstract AssertionType getType();

	/**
	 * Creates new class assertion.
	 * 
	 * @param assertionIdentifier
	 *            Assertion identifier
	 * @param isInferred
	 *            Whether the assertion uses inferred values
	 * @return Assertion
	 */
	public static Assertion createClassAssertion(URI assertionIdentifier, boolean isInferred) {
		return new ClassAssertion(assertionIdentifier, isInferred);
	}

	/**
	 * Creates new object property assertion.
	 * 
	 * @param assertionIdentifier
	 *            Assertion identifier
	 * @param isInferred
	 *            Whether the assertion uses inferred values
	 * @return Assertion
	 */
	public static Assertion createObjectPropertyAssertion(URI assertionIdentifier,
			boolean isInferred) {
		return new ObjectPropertyAssertion(assertionIdentifier, isInferred);
	}

	/**
	 * Creates new data property assertion.
	 * 
	 * @param assertionIdentifier
	 *            Assertion identifier
	 * @param isInferred
	 *            Whether the assertion uses inferred values
	 * @return Assertion
	 */
	public static Assertion createDataPropertyAssertion(URI assertionIdentifier, boolean isInferred) {
		return new DataPropertyAssertion(assertionIdentifier, isInferred);
	}

	/**
	 * Creates new annotation property assertion.
	 * 
	 * @param assertionIdentifier
	 *            Assertion identifier
	 * @param isInferred
	 *            Whether the assertion uses inferred values
	 * @return Assertion
	 */
	public static Assertion createAnnotationPropertyAssertion(URI assertionIdentifier,
			boolean isInferred) {
		return new AnnotationPropertyAssertion(assertionIdentifier, isInferred);
	}
}
