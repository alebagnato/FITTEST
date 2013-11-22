package fbk.se.mutation.utils;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import com.fasterxml.jackson.databind.JsonNode;
import fbk.se.mutation.xpath.VerySimpleXpathParser;

public class JsonUtils {
	
	/**
	 * Map path to tree nodes, each segment to a set of nodes of a same level from the root
	 * 
	 * @param path
	 * @param treeRoot
	 * @return
	 * @throws PathException
	 */
	public static Map<String, List<JsonNode>> mapPath2Tree(String path, JsonNode treeRoot) throws PathException {
		
		List<String> pathSegments = parsePath(path);
		if(pathSegments == null) return null;
		
		return mapPath2Tree(pathSegments, treeRoot);
	}
	
	/**
	 * Map path segments to tree nodes, each segment to a set of nodes of a same level from the root
	 * @param pathSegments
	 * @param treeRoot
	 * @return
	 * @throws PathException
	 */
	public static Map<String, List<JsonNode>> mapPath2Tree(List<String> pathSegments, JsonNode treeRoot) throws PathException {
		
		if (treeRoot == null 
				|| !treeRoot.has(pathSegments.get(0))){
			// Failed from root
			return null;
		}
		Map<String, List<JsonNode>> queryMap = new HashMap<String, List<JsonNode>>();
 		for (String s : pathSegments){
			queryMap.put(s, new ArrayList<JsonNode>());
		}
		
 		
 		for (int i = 0; i < pathSegments.size(); i++){
			String nodeName = pathSegments.get(i);
			List<JsonNode> ret = new ArrayList<JsonNode>();
			if (i == 0){
				JsonNode childs = treeRoot.get(nodeName);
				if (childs != null){
					if (childs.isArray()){
						Iterator<JsonNode> iter = childs.iterator();
						while (iter.hasNext()){
							ret.add(iter.next());
						}
					} else {
						ret.add(childs);
					}
				}
			} else {
				// parent node
				List<JsonNode> parents = queryMap.get(pathSegments.get(i-1));
				for (JsonNode parent : parents){
					JsonNode childs = parent.get(nodeName);
					if (childs != null){
						if (childs.isArray()){
							Iterator<JsonNode> iter = childs.iterator();
							while (iter.hasNext()){
								ret.add(iter.next());
							}
						} else {
							ret.add(childs);
						}
					}
				}
			}
			if (ret.size() > 0){
				queryMap.get(nodeName).addAll(ret);
			} else if (i < pathSegments.size() - 1) {
				// not found in the middle of the path
				return null;
			}
 		}
 		
 		return queryMap;
 		
	}

	/**
	 * Find nodes that match the path query, like the simplest form of xpath 
	 * @param xpath
	 * @param treeRoot
	 * @return
	 * @throws PathException 
	 */
	public static List<JsonNode> query(String path, JsonNode treeRoot) throws PathException {
		
		List<String> pathSegments = parsePath(path);

		Map<String, List<JsonNode>> queryMap = mapPath2Tree(pathSegments, treeRoot);
		if (queryMap == null) return null;
		
 		List<JsonNode> retList = queryMap.get(pathSegments.get(pathSegments.size() - 1));
 		if (retList.size() > 0){
 			return retList;
 		} else {
 			return null;
 		}
	}
	
	/**
	 * Parse the path expression
	 * @param path
	 * @return
	 * 	- a List<String> of path segments if the path expression is correct
	 *  - null, otherwise
	 * @throws PathException
	 */
	private static List<String> parsePath(String path) throws PathException{
		VerySimpleXpathParser pathParser  = new VerySimpleXpathParser(path);
		List<String> pathSegments = pathParser.getFullPath();
		
		if (pathSegments == null){
			throw new PathException("Problem with the path exception, should be something" +
					" like /root/first/second/field");
		}
		if (pathSegments.size() == 0){
			return null;
		}
		
		return pathSegments;
	}
	
}
