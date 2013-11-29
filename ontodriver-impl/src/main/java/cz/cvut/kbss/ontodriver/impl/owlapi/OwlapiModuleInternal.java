package cz.cvut.kbss.ontodriver.impl.owlapi;

import java.util.List;

import org.semanticweb.owlapi.model.OWLOntologyChange;

import cz.cvut.kbss.ontodriver.ResultSet;
import cz.cvut.kbss.ontodriver.impl.ModuleInternal;

interface OwlapiModuleInternal extends ModuleInternal {

	/**
	 * Retrieves changes performed since the last {@code commit} and resets the
	 * change list. </p>
	 * 
	 * All changes that were applied since the last {@code commit} are returned
	 * and the list that tracks changes in this ModuleInternal is reset.
	 * 
	 * @return List of changes applied since last call of this method
	 */
	public List<OWLOntologyChange> commitAndRetrieveChanges();

	/**
	 * Executes the specified SPARQL statement.
	 * 
	 * @param statement
	 *            the statement to execute
	 * @return ResultSet containing evaluation results
	 */
	public ResultSet executeStatement(OwlapiStatement statement);
}
