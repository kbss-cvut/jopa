package cz.cvut.kbss.ontodriver;

/**
 * Represents expressiveness of a Context.
 * 
 * @author kidney
 * 
 */
public enum ContextExpressiveness {
	OWL2FULL, OWL2DL, OWL2EL, OWL2QL, OWL2RL, RDFS, UNKNOWN
	// TODO Should this contain names of the profiles - OWL DL, OWL RL, RDFS
	// etc. or descriptive logic types - SHOIN etc.?
}
