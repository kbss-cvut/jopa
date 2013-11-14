package cz.cvut.kbss.ontodriver;

public interface DriverStatement {

	/**
	 * Executes the query represented by this statement.
	 * 
	 * @return Result set
	 */
	public ResultSet executeStatement();

}
