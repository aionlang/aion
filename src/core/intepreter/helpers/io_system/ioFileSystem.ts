import { IOSystem } from "./ioSystem";
import * as fs from "fs";

export class IOFileSystem implements IOSystem {
    importFile(path: string): string {
        try {
            return fs.readFileSync(path, "utf-8");
        } catch (error) {
            throw new Error(`Error reading file ${path}: ${error}`);
        }
    }

    saveFile(fileName: string, fileContents: string): boolean {
        try {
            fs.writeFileSync(fileName, fileContents, "utf-8");
            return true;
        } catch (error) {
            return false; 
        }
    }
}
