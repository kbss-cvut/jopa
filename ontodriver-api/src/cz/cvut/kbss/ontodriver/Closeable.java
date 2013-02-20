package cz.cvut.kbss.ontodriver;

/**
 * Interface for closeable resources.
 * 
 * @author kidney
 * 
 */
public interface Closeable {

	/**
	 * Closes this resource releasing any sub-resources it holds. </p>
	 * 
	 * After closing the resource is not usable any more and calling methods on
	 * it (except {@code close} and {@code isOpen}) will result in
	 * {@code OntoDriverException}. </p>
	 * 
	 * Calling {@code close} on already closed resource does nothing.
	 * 
	 * @throws OntoDriverException
	 *             If an ontology access error occurs.
	 */
	public void close() throws OntoDriverException;

	/**
	 * Retrieves status of this resource. </p>
	 * 
	 * @return {@code true} if the resource is open, {@code false} otherwise
	 */
	public boolean isOpen();

}
