// To parse this JSON:
//
//   NSError *error;
//   QTPostback *postback = [QTPostback fromJSON:json encoding:NSUTF8Encoding error:&error];

#import <Foundation/Foundation.h>

@class QTPostback;
@class QTDefinitions;
@class QTMeta;
@class QTPostbackClass;
@class QTProperties;
@class QTAppID;
@class QTType;
@class QTTimestamp;
@class QTMetaClass;

NS_ASSUME_NONNULL_BEGIN

#pragma mark - Boxed enums

@interface QTType : NSObject
@property (nonatomic, readonly, copy) NSString *value;
+ (instancetype _Nullable)withValue:(NSString *)value;
+ (QTType *)integer;
+ (QTType *)null;
+ (QTType *)string;
@end

#pragma mark - Object interfaces

@interface QTPostback : NSObject
@property (nonatomic, copy)   NSString *ref;
@property (nonatomic, copy)   NSString *schema;
@property (nonatomic, strong) QTDefinitions *definitions;

+ (_Nullable instancetype)fromJSON:(NSString *)json encoding:(NSStringEncoding)encoding error:(NSError *_Nullable *)error;
+ (_Nullable instancetype)fromData:(NSData *)data error:(NSError *_Nullable *)error;
- (NSString *_Nullable)toJSON:(NSStringEncoding)encoding error:(NSError *_Nullable *)error;
- (NSData *_Nullable)toData:(NSError *_Nullable *)error;
@end

@interface QTDefinitions : NSObject
@property (nonatomic, strong) QTMeta *meta;
@property (nonatomic, strong) QTPostbackClass *postback;
@end

@interface QTMeta : NSObject
@property (nonatomic, assign) BOOL isAdditionalProperties;
@property (nonatomic, copy)   NSString *title;
@property (nonatomic, copy)   NSString *type;
@end

@interface QTPostbackClass : NSObject
@property (nonatomic, assign) BOOL isAdditionalProperties;
@property (nonatomic, strong) QTProperties *properties;
@property (nonatomic, copy)   NSArray<NSString *> *required;
@property (nonatomic, copy)   NSString *title;
@property (nonatomic, copy)   NSString *type;
@end

@interface QTProperties : NSObject
@property (nonatomic, strong) QTAppID *appID;
@property (nonatomic, strong) QTAppID *averagePrice;
@property (nonatomic, strong) QTAppID *cancelledQuantity;
@property (nonatomic, strong) QTAppID *checksum;
@property (nonatomic, strong) QTAppID *disclosedQuantity;
@property (nonatomic, strong) QTAppID *exchange;
@property (nonatomic, strong) QTAppID *exchangeOrderID;
@property (nonatomic, strong) QTTimestamp *exchangeTimestamp;
@property (nonatomic, strong) QTTimestamp *exchangeUpdateTimestamp;
@property (nonatomic, strong) QTAppID *filledQuantity;
@property (nonatomic, strong) QTAppID *guid;
@property (nonatomic, strong) QTAppID *instrumentToken;
@property (nonatomic, strong) QTAppID *marketProtection;
@property (nonatomic, strong) QTMetaClass *meta;
@property (nonatomic, strong) QTAppID *orderID;
@property (nonatomic, strong) QTTimestamp *orderTimestamp;
@property (nonatomic, strong) QTAppID *orderType;
@property (nonatomic, strong) QTAppID *parentOrderID;
@property (nonatomic, strong) QTAppID *pendingQuantity;
@property (nonatomic, strong) QTAppID *placedBy;
@property (nonatomic, strong) QTAppID *price;
@property (nonatomic, strong) QTAppID *product;
@property (nonatomic, strong) QTAppID *quantity;
@property (nonatomic, strong) QTAppID *status;
@property (nonatomic, strong) QTAppID *statusMessage;
@property (nonatomic, strong) QTAppID *statusMessageRaw;
@property (nonatomic, strong) QTAppID *tag;
@property (nonatomic, strong) QTAppID *tradingsymbol;
@property (nonatomic, strong) QTAppID *transactionType;
@property (nonatomic, strong) QTAppID *triggerPrice;
@property (nonatomic, strong) QTAppID *unfilledQuantity;
@property (nonatomic, strong) QTAppID *userID;
@property (nonatomic, strong) QTAppID *validity;
@property (nonatomic, strong) QTAppID *variety;
@end

@interface QTAppID : NSObject
@property (nonatomic, assign) QTType *type;
@end

@interface QTTimestamp : NSObject
@property (nonatomic, copy)   NSString *format;
@property (nonatomic, assign) QTType *type;
@end

@interface QTMetaClass : NSObject
@property (nonatomic, copy) NSString *ref;
@end

NS_ASSUME_NONNULL_END
