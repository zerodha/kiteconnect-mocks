// To parse this JSON:
//
//   NSError *error;
//   QTConvertPosition *convertPosition = [QTConvertPosition fromJSON:json encoding:NSUTF8Encoding error:&error];

#import <Foundation/Foundation.h>

@class QTConvertPosition;
@class QTDefinitions;
@class QTConvertPositionClass;
@class QTProperties;
@class QTData;

NS_ASSUME_NONNULL_BEGIN

#pragma mark - Object interfaces

@interface QTConvertPosition : NSObject
@property (nonatomic, copy)   NSString *ref;
@property (nonatomic, copy)   NSString *schema;
@property (nonatomic, strong) QTDefinitions *definitions;

+ (_Nullable instancetype)fromJSON:(NSString *)json encoding:(NSStringEncoding)encoding error:(NSError *_Nullable *)error;
+ (_Nullable instancetype)fromData:(NSData *)data error:(NSError *_Nullable *)error;
- (NSString *_Nullable)toJSON:(NSStringEncoding)encoding error:(NSError *_Nullable *)error;
- (NSData *_Nullable)toData:(NSError *_Nullable *)error;
@end

@interface QTDefinitions : NSObject
@property (nonatomic, strong) QTConvertPositionClass *convertPosition;
@end

@interface QTConvertPositionClass : NSObject
@property (nonatomic, assign) BOOL isAdditionalProperties;
@property (nonatomic, strong) QTProperties *properties;
@property (nonatomic, copy)   NSArray<NSString *> *required;
@property (nonatomic, copy)   NSString *title;
@property (nonatomic, copy)   NSString *type;
@end

@interface QTProperties : NSObject
@property (nonatomic, strong) QTData *data;
@property (nonatomic, strong) QTData *status;
@end

@interface QTData : NSObject
@property (nonatomic, copy) NSString *type;
@end

NS_ASSUME_NONNULL_END
